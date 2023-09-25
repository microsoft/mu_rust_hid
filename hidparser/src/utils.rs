//! Parser Utility Functions
//!
//! This module contains utility functions used to do conversions of variable length byte slices into integer types.
//!
//! ## License
//!
//! Copyright (C) Microsoft Corporation. All rights reserved.
//!
//! SPDX-License-Identifier: BSD-2-Clause-Patent
//!
pub fn u16_from_bytes(bytes: &[u8]) -> u16 {
  let mut u16_bytes: [u8; 2] = [0; 2];
  u16_bytes[..bytes.len()].clone_from_slice(bytes);
  u16::from_le_bytes(u16_bytes)
}

pub fn u32_from_bytes(bytes: &[u8]) -> u32 {
  let mut u32_bytes: [u8; 4] = [0; 4];
  u32_bytes[..bytes.len()].clone_from_slice(bytes);
  u32::from_le_bytes(u32_bytes)
}

pub fn i32_from_bytes(bytes: &[u8]) -> i32 {
  let mut i32_bytes: [u8; 4] = [0; 4];
  //sign-extend
  if (bytes.last().unwrap() & 0x80) != 0 {
    i32_bytes.fill(0xff);
  }
  i32_bytes[..bytes.len()].clone_from_slice(bytes);
  i32::from_le_bytes(i32_bytes)
}
