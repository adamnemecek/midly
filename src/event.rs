#[cfg(feature = "std")]
use crate::primitive::write_varlen_slice;
use crate::{
    prelude::*,
    primitive::{read_varlen_slice, SmpteTime},
};

/// Represents a fully parsed track event, with delta time.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Event<'a> {
    pub delta: u28,
    pub kind: EventKind<'a>,
}
impl<'a> Event<'a> {
    /// Read an `Smf` track event from raw track data.
    ///
    /// The received raw slice should extend to the very end of the track.
    ///
    /// The first return value is a prefix of the input slice, containing the bytes that form
    /// the first event in the track.
    /// The second return value is this very same event, but parsed.
    ///
    /// The `raw` slice will be modified and have this prefix removed.
    pub fn read(raw: &mut &'a [u8], running_status: &mut Option<u8>) -> Result<(&'a [u8], Self)> {
        let delta = u28::read_u7(raw).context(err_invalid("failed to read event deltatime"))?;
        let (raw, kind) =
            EventKind::read(raw, running_status).context(err_invalid("failed to parse event"))?;
        Ok((raw, Event { delta, kind }))
    }
    #[cfg(feature = "std")]
    pub(crate) fn write<W: Write>(
        &self,
        running_status: &mut Option<u8>,
        out: &mut W,
    ) -> IoResult<()> {
        self.delta.write_varlen(out)?;
        self.kind.write(running_status, out)?;
        Ok(())
    }
}

/// Represents the different kinds of events.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum EventKind<'a> {
    /// A standard MIDI message bound to a channel.
    Midi { channel: u4, message: MidiMessage },
    /// A System Exclusive message, carrying arbitrary data.
    SysEx(&'a [u8]),
    /// An escape sequence, intended to send arbitrary data to the MIDI synthesizer.
    Escape(&'a [u8]),
    /// A meta-message, giving extra information for correct playback, like tempo, song name,
    /// lyrics, etc...
    Meta(MetaMessage<'a>),
}
impl<'a> EventKind<'a> {
    /// Reads a single event from the given stream.
    /// Use this method when reading raw MIDI messages from a stream.
    ///
    /// Returns the event read and the raw bytes that make up the event, taken directly from the
    /// source bytes.
    ///
    /// The running status byte is used to fill in any missing message status (a process known
    /// as "running status").
    /// This running status should be conserved across calls to `EventKind::parse`, and should be
    /// unique per-midi-stream.
    /// Initially it should be set to `None`.
    ///
    /// This method takes a *mutable reference* to a byteslice and a running status byte.
    /// In case of success the byteslice is advanced to the next event, and the running status
    /// might be changed to a new status.
    /// In case of error no changes are made to these values.
    pub fn parse(raw: &mut &'a [u8], running_status: &mut Option<u8>) -> Result<(&'a [u8], Self)> {
        let (old_raw, old_rs) = (*raw, *running_status);
        let maybe_ev = Self::read(raw, running_status);
        if let Err(_) = maybe_ev {
            *raw = old_raw;
            *running_status = old_rs;
        }
        maybe_ev
    }

    fn read(raw: &mut &'a [u8], running_status: &mut Option<u8>) -> Result<(&'a [u8], Self)> {
        //Keep the beggining of the old slice
        let old_slice = *raw;
        //Read status
        let mut status = *raw.get(0).ok_or(err_invalid("failed to read status"))?;
        if status < 0x80 {
            //Running status!
            status = running_status.ok_or(err_invalid(
                "event missing status with no running status active",
            ))?;
        } else {
            //Advance slice 1 byte to consume status. Note that because we already did `get()`, we
            //can use panicking index here
            *raw = &raw[1..];
        }
        //Delegate further parsing depending on status
        let kind = match status {
            0x80..=0xEF => {
                *running_status = Some(status);
                let channel = u4::from(bit_range(status, 0..4));
                Self::Midi {
                    channel,
                    message: MidiMessage::read(raw, status)
                        .context(err_invalid("failed to read midi message"))?,
                }
            }
            0xF0 => {
                *running_status = None;
                Self::SysEx(
                    read_varlen_slice(raw).context(err_invalid("failed to read sysex event"))?,
                )
            }
            0xF7 => {
                *running_status = None;
                Self::Escape(
                    read_varlen_slice(raw).context(err_invalid("failed to read escape event"))?,
                )
            }
            0xFF => Self::Meta(
                MetaMessage::read(raw).context(err_invalid("failed to read meta event"))?,
            ),
            _ => bail!(err_invalid("invalid event status")),
        };
        //The `raw` slice has moved forward by exactly the amount of bytes that form this midi
        //event
        //Therefore the source slice can be determined by rescuing these consumed bytes
        let len = raw.as_ptr() as usize - old_slice.as_ptr() as usize;
        let source_bytes = &old_slice[0..len];
        Ok((source_bytes, kind))
    }

    /// Writes a single event to the given output writer.
    ///
    /// `running_status` keeps track of the last MIDI status, in order to make proper use of
    /// running status. It should be shared between sequential calls, and should initially be set
    /// to `None`. If you wish to disable running status, pass in `&mut None` to every call to
    /// this method.
    #[cfg(feature = "std")]
    pub fn write<W: Write>(&self, running_status: &mut Option<u8>, out: &mut W) -> IoResult<()> {
        //Running Status rules:
        // - MIDI Messages (0x80 ..= 0xEF) alter and use running status
        // - System Common (0xF0 ..= 0xF7) cancel and cannot use running status
        // - System Realtime (0xF8 ..= 0xFF), including Meta Messages, do not alter running status
        //      and cannot use it either
        match self {
            Self::Midi { channel, message } => {
                let status = message.status_nibble() << 4 | channel.as_int();
                if Some(status) != *running_status {
                    //Explicitly write status
                    out.write_all(&[status])?;
                    *running_status = Some(status);
                }
                message.write(out)?;
            }
            Self::SysEx(data) => {
                *running_status = None;
                out.write_all(&[0xF0])?;
                write_varlen_slice(data, out)?;
            }
            Self::Escape(data) => {
                *running_status = None;
                out.write_all(&[0xF7])?;
                write_varlen_slice(data, out)?;
            }
            Self::Meta(meta) => {
                out.write_all(&[0xFF])?;
                meta.write(out)?;
            }
        }
        Ok(())
    }
}

/// Represents a MIDI message, not an event.
///
/// If reading a MIDI message from some stream, use `EventKind::read` instead and discard non-midi
/// events.
/// This is the correct way to handle running status.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum MidiMessage {
    /// Stop playing a note.
    NoteOff {
        /// The MIDI key to stop playing.
        key: u7,
        /// The velocity with which to stop playing it.
        vel: u7,
    },
    /// Start playing a note.
    NoteOn {
        /// The key to start playing.
        key: u7,
        /// The velocity (strength) with which to press it.
        vel: u7,
    },
    /// Modify the velocity of a note after it has been played.
    Aftertouch {
        /// The key for which to modify its velocity.
        key: u7,
        /// The new velocity for the key.
        vel: u7,
    },
    /// Modify the value of a MIDI controller.
    Controller {
        /// The controller to modify.
        ///
        /// See the MIDI spec for the meaning of each index.
        controller: u7,
        /// The value to set it to.
        value: u7,
    },
    /// Change the program (also known as instrument) for a channel.
    ProgramChange {
        /// The new program (instrument) to use for the channel.
        program: u7,
    },
    /// Change the note velocity of a whole channel at once, without starting new notes.
    ChannelAftertouch {
        /// The new velocity for the notes currently playing in the channel.
        vel: u7,
    },
    /// Set the pitch bend value.
    PitchBend {
        /// The new pitch-bend value.
        ///
        /// A value of `0x0000` indicates full bend downwards.
        /// A value of `0x2000` indicates no bend.
        /// A value of `0x3FFF` indicates full bend upwards.
        bend: u14,
    },
}
impl MidiMessage {
    /// Receives a slice pointing to midi args (not including status byte)
    /// Status byte is given separately to reuse running status
    fn read(raw: &mut &[u8], status: u8) -> Result<Self> {
        Ok(match bit_range(status, 4..8) {
            0x8 => Self::NoteOff {
                key: u7::read(raw)?,
                vel: u7::read(raw)?,
            },
            0x9 => Self::NoteOn {
                key: u7::read(raw)?,
                vel: u7::read(raw)?,
            },
            0xA => Self::Aftertouch {
                key: u7::read(raw)?,
                vel: u7::read(raw)?,
            },
            0xB => Self::Controller {
                controller: u7::read(raw)?,
                value: u7::read(raw)?,
            },
            0xC => Self::ProgramChange {
                program: u7::read(raw)?,
            },
            0xD => Self::ChannelAftertouch {
                vel: u7::read(raw)?,
            },
            0xE => {
                //Note the little-endian order, contrasting with the default big-endian order of
                //Standard Midi Files
                let lsb = u7::read(raw)?.as_int() as u16;
                let msb = u7::read(raw)?.as_int() as u16;
                Self::PitchBend {
                    bend: u14::from(msb << 7 | lsb),
                }
            }
            _ => bail!(err_invalid("invalid midi message status")),
        })
    }
    /// Get the raw status nibble for this MIDI message type.
    #[cfg(feature = "std")]
    fn status_nibble(&self) -> u8 {
        match self {
            Self::NoteOff { .. } => 0x8,
            Self::NoteOn { .. } => 0x9,
            Self::Aftertouch { .. } => 0xA,
            Self::Controller { .. } => 0xB,
            Self::ProgramChange { .. } => 0xC,
            Self::ChannelAftertouch { .. } => 0xD,
            Self::PitchBend { .. } => 0xE,
        }
    }
    #[cfg(feature = "std")]
    fn write<W: Write>(&self, out: &mut W) -> IoResult<()> {
        match self {
            Self::NoteOff { key, vel } => out.write_all(&[key.as_int(), vel.as_int()])?,
            Self::NoteOn { key, vel } => out.write_all(&[key.as_int(), vel.as_int()])?,
            Self::Aftertouch { key, vel } => out.write_all(&[key.as_int(), vel.as_int()])?,
            Self::Controller { controller, value } => {
                out.write_all(&[controller.as_int(), value.as_int()])?
            }
            Self::ProgramChange { program } => out.write_all(&[program.as_int()])?,
            Self::ChannelAftertouch { vel } => out.write_all(&[vel.as_int()])?,
            Self::PitchBend { bend } => {
                out.write_all(&[(bend.as_int() & 0x7F) as u8, (bend.as_int() >> 7) as u8])?
            }
        }
        Ok(())
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum MetaMessage<'a> {
    /// For `Format::Sequential` MIDI file types, `TrackNumber` can be empty, and defaults to
    /// track index.
    TrackNumber(Option<u16>),
    Text(&'a [u8]),
    Copyright(&'a [u8]),
    TrackName(&'a [u8]),
    InstrumentName(&'a [u8]),
    Lyric(&'a [u8]),
    Marker(&'a [u8]),
    CuePoint(&'a [u8]),
    ProgramName(&'a [u8]),
    DeviceName(&'a [u8]),
    MidiChannel(u4),
    MidiPort(u7),
    /// Obligatory at track end.
    EndOfTrack,
    /// Amount of microseconds per beat (quarter note).
    ///
    /// Usually appears at the beggining of a track, before any midi events are sent, but there
    /// are no guarantees.
    Tempo(u24),
    SmpteOffset(SmpteTime),
    /// In order of the MIDI specification, numerator, denominator, midi clocks per click, 32nd
    /// notes per quarter
    TimeSignature(u8, u8, u8, u8),
    /// As in the MIDI specification, negative numbers indicate number of flats and positive
    /// numbers indicate number of sharps.
    /// `false` indicates a major scale, `true` indicates a minor scale.
    KeySignature(i8, bool),
    SequencerSpecific(&'a [u8]),
    /// An unknown meta-message, unconforming to the spec.
    ///
    /// This event is not generated with the `strict` feature enabled.
    Unknown(u8, &'a [u8]),
}
impl<'a> MetaMessage<'a> {
    fn read(raw: &mut &'a [u8]) -> Result<Self> {
        let type_byte = u8::read(raw).context(err_invalid("failed to read meta message type"))?;
        let mut data =
            read_varlen_slice(raw).context(err_invalid("failed to read meta message data"))?;
        Ok(match type_byte {
            0x00 => Self::TrackNumber({
                if cfg!(feature = "strict") {
                    ensure!(
                        data.len() == 0 || data.len() == 2,
                        err_pedantic("invalid tracknumber event length")
                    );
                }
                if data.len() >= 2 {
                    Some(u16::read(&mut data)?)
                } else {
                    None
                }
            }),
            0x01 => Self::Text(data),
            0x02 => Self::Copyright(data),
            0x03 => Self::TrackName(data),
            0x04 => Self::InstrumentName(data),
            0x05 => Self::Lyric(data),
            0x06 => Self::Marker(data),
            0x07 => Self::CuePoint(data),
            0x08 => Self::ProgramName(data),
            0x09 => Self::DeviceName(data),
            0x20 if data.len() >= 1 => {
                if cfg!(feature = "strict") {
                    ensure!(
                        data.len() == 1,
                        err_pedantic("invalid midichannel event length")
                    );
                }
                Self::MidiChannel(u4::read(&mut data)?)
            }
            0x21 if data.len() >= 1 => {
                if cfg!(feature = "strict") {
                    ensure!(
                        data.len() == 1,
                        err_pedantic("invalid midiport event length")
                    );
                }
                Self::MidiPort(u7::read(&mut data)?)
            }
            0x2F => {
                if cfg!(feature = "strict") {
                    ensure!(
                        data.len() == 0,
                        err_pedantic("invalid endoftrack event length")
                    );
                }
                Self::EndOfTrack
            }
            0x51 if data.len() >= 3 => {
                if cfg!(feature = "strict") {
                    ensure!(data.len() == 3, err_pedantic("invalid tempo event length"));
                }
                Self::Tempo(u24::read(&mut data)?)
            }
            0x54 if data.len() >= 5 => {
                if cfg!(feature = "strict") {
                    ensure!(
                        data.len() == 5,
                        err_pedantic("invalid smpteoffset event length")
                    );
                }
                Self::SmpteOffset(
                    SmpteTime::read(&mut data).context(err_invalid("failed to read smpte time"))?,
                )
            }
            0x58 if data.len() >= 4 => {
                if cfg!(feature = "strict") {
                    ensure!(
                        data.len() == 4,
                        err_pedantic("invalid timesignature event length")
                    );
                }
                Self::TimeSignature(
                    u8::read(&mut data)?,
                    u8::read(&mut data)?,
                    u8::read(&mut data)?,
                    u8::read(&mut data)?,
                )
            }
            0x59 => {
                Self::KeySignature(u8::read(&mut data)? as i8, u8::read(&mut data)? != 0)
            }
            0x7F => Self::SequencerSpecific(data),
            _ => {
                if cfg!(feature = "strict") {
                    bail!(err_pedantic("unknown meta event type"))
                } else {
                    Self::Unknown(type_byte, data)
                }
            }
        })
    }
    #[cfg(feature = "std")]
    fn write<W: Write>(&self, out: &mut W) -> IoResult<()> {
        let mut write_msg = |type_byte: u8, data: &[u8]| {
            out.write_all(&[type_byte])?;
            write_varlen_slice(data, out)?;
            Ok(())
        };
        match self {
            Self::TrackNumber(track_num) => match track_num {
                None => write_msg(0x00, &[]),
                Some(track_num) => write_msg(0x00, &track_num.to_be_bytes()[..]),
            },
            Self::Text(data) => write_msg(0x01, data),
            Self::Copyright(data) => write_msg(0x02, data),
            Self::TrackName(data) => write_msg(0x03, data),
            Self::InstrumentName(data) => write_msg(0x04, data),
            Self::Lyric(data) => write_msg(0x05, data),
            Self::Marker(data) => write_msg(0x06, data),
            Self::CuePoint(data) => write_msg(0x07, data),
            Self::ProgramName(data) => write_msg(0x08, data),
            Self::DeviceName(data) => write_msg(0x09, data),
            Self::MidiChannel(chan) => write_msg(0x20, &[chan.as_int()]),
            Self::MidiPort(port) => write_msg(0x21, &[port.as_int()]),
            Self::EndOfTrack => write_msg(0x2F, &[]),
            Self::Tempo(microsperbeat) => {
                write_msg(0x51, &microsperbeat.as_int().to_be_bytes()[1..])
            }
            Self::SmpteOffset(smpte) => write_msg(0x54, &smpte.encode()[..]),
            Self::TimeSignature(num, den, ticksperclick, thirtysecondsperquarter) => {
                write_msg(
                    0x58,
                    &[*num, *den, *ticksperclick, *thirtysecondsperquarter],
                )
            }
            Self::KeySignature(sharps, minor) => {
                write_msg(0x59, &[*sharps as u8, *minor as u8])
            }
            Self::SequencerSpecific(data) => write_msg(0x7F, data),
            Self::Unknown(type_byte, data) => write_msg(*type_byte, data),
        }
    }
}
