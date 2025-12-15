#![allow(dead_code)]

/// Simple stub garbage collector used to make the VM API compile.
#[derive(Debug, Clone, Copy)]
pub enum GCStrategy {
    Generational,
}

#[derive(Debug, Default)]
pub struct GarbageCollector {
    memory_usage: usize,
    collections: usize,
    freed_objects: usize,
}

impl GarbageCollector {
    pub fn new(_strategy: GCStrategy) -> Self {
        Self::default()
    }

    pub fn get_memory_usage(&self) -> usize {
        self.memory_usage
    }

    pub fn get_stats(&self) -> (usize, usize) {
        (self.collections, self.freed_objects)
    }

    pub fn reset(&mut self) {
        self.memory_usage = 0;
        self.collections = 0;
        self.freed_objects = 0;
    }
}


