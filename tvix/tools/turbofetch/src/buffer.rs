use magic_buffer::MagicBuffer;
use std::cell::Cell;

pub struct Buffer {
    buffer: MagicBuffer,
    /// first readable byte
    head: Cell<usize>,
    /// first writable byte
    tail: usize,
}

unsafe impl Send for Buffer {}

impl Buffer {
    pub fn new(len: usize) -> Buffer {
        Buffer {
            // MagicBuffer::new verifies that `len` is a power of two
            buffer: MagicBuffer::new(len).unwrap(),
            // head == tail means the buffer is empty
            // the buffer can only be filled with len-1 bytes
            // in order to ensure that this remains unambiguous
            head: Cell::new(0),
            tail: 0,
        }
    }

    /// return the valid, readable data in the buffer
    pub fn data(&self) -> &[u8] {
        let len = self.buffer.len();
        let head = self.head.get();

        if head <= self.tail {
            &self.buffer[head..self.tail]
        } else {
            &self.buffer[head..self.tail + len]
        }
    }

    /// mark `read_len` bytes of the readable data as consumed, freeing the space
    pub fn consume(&self, read_len: usize) {
        debug_assert!(read_len <= self.data().len());
        let mut head = self.head.get();
        head += read_len;
        head &= self.buffer.len() - 1;
        self.head.set(head);
    }

    /// return the empty, writable space in the buffer
    pub fn space(&mut self) -> &mut [u8] {
        let len = self.buffer.len();
        let head = self.head.get();

        if head <= self.tail {
            &mut self.buffer[self.tail..head + len - 1]
        } else {
            &mut self.buffer[self.tail..head - 1]
        }
    }

    /// mark `written_len` bytes of the writable space as valid, readable data
    pub fn commit(&mut self, written_len: usize) {
        debug_assert!(written_len <= self.space().len());
        self.tail += written_len;
        self.tail &= self.buffer.len() - 1;
    }
}
