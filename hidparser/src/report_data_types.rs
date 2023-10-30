//! Report Descriptor Data Types
//!
//! This module contains structures that are used to describe a report.
//!
//! ## License
//!
//! Copyright (C) Microsoft Corporation. All rights reserved.
//!
//! SPDX-License-Identifier: BSD-2-Clause-Patent
//!
use crate::utils::{i32_from_bytes, u16_from_bytes, u32_from_bytes};
use core::ops::RangeInclusive;

/// Usage page global item data type.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct UsagePage(u16);
impl From<&[u8]> for UsagePage {
  fn from(bytes: &[u8]) -> Self {
    UsagePage(u16_from_bytes(bytes))
  }
}

impl From<u16> for UsagePage {
  fn from(val: u16) -> Self {
    UsagePage(val)
  }
}

/// Logical minimum global item data type.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct LogicalMinimum(i32);
impl From<&[u8]> for LogicalMinimum {
  fn from(bytes: &[u8]) -> Self {
    LogicalMinimum(i32_from_bytes(bytes))
  }
}
impl From<i32> for LogicalMinimum {
  fn from(val: i32) -> Self {
    LogicalMinimum(val)
  }
}
impl From<LogicalMinimum> for i32 {
  fn from(val: LogicalMinimum) -> Self {
    val.0
  }
}
impl From<LogicalMinimum> for u32 {
  fn from(val: LogicalMinimum) -> Self {
    val.0 as u32
  }
}

/// Logical maximum global item data type.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct LogicalMaximum(i32);
impl From<&[u8]> for LogicalMaximum {
  fn from(bytes: &[u8]) -> Self {
    LogicalMaximum(i32_from_bytes(bytes))
  }
}
impl From<i32> for LogicalMaximum {
  fn from(val: i32) -> Self {
    LogicalMaximum(val)
  }
}
impl From<u32> for LogicalMaximum {
  fn from(val: u32) -> Self {
    LogicalMaximum(val as i32)
  }
}
impl From<LogicalMaximum> for i32 {
  fn from(val: LogicalMaximum) -> Self {
    val.0
  }
}
impl From<LogicalMaximum> for u32 {
  fn from(val: LogicalMaximum) -> Self {
    val.0 as u32
  }
}

/// Physical minimum global item data type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PhysicalMinimum(i32);
impl From<&[u8]> for PhysicalMinimum {
  fn from(bytes: &[u8]) -> Self {
    PhysicalMinimum(i32_from_bytes(bytes))
  }
}
impl From<i32> for PhysicalMinimum {
  fn from(val: i32) -> Self {
    PhysicalMinimum(val)
  }
}

/// Physical maximum global item data type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PhysicalMaximum(i32);
impl From<&[u8]> for PhysicalMaximum {
  fn from(bytes: &[u8]) -> Self {
    PhysicalMaximum(i32_from_bytes(bytes))
  }
}
impl From<i32> for PhysicalMaximum {
  fn from(val: i32) -> Self {
    PhysicalMaximum(val)
  }
}

/// Unit exponent global item data type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UnitExponent(u32);
impl From<&[u8]> for UnitExponent {
  fn from(bytes: &[u8]) -> Self {
    UnitExponent(u32_from_bytes(bytes))
  }
}
impl From<u32> for UnitExponent {
  fn from(val: u32) -> Self {
    UnitExponent(val)
  }
}

/// Unit global item data type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Unit(u32);
impl From<&[u8]> for Unit {
  fn from(bytes: &[u8]) -> Self {
    Unit(u32_from_bytes(bytes))
  }
}
impl From<u32> for Unit {
  fn from(val: u32) -> Self {
    Unit(val)
  }
}
/// Report Size global item data type.
#[derive(Debug, Clone, Copy)]
pub struct ReportSize(u32);
impl From<&[u8]> for ReportSize {
  fn from(bytes: &[u8]) -> Self {
    ReportSize(u32_from_bytes(bytes))
  }
}
impl From<u32> for ReportSize {
  fn from(val: u32) -> Self {
    ReportSize(val)
  }
}
impl From<ReportSize> for u32 {
  fn from(val: ReportSize) -> Self {
    val.0
  }
}

/// Report Id global item data type.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct ReportId(u32);
impl From<&[u8]> for ReportId {
  fn from(bytes: &[u8]) -> Self {
    ReportId(u32_from_bytes(bytes))
  }
}
impl From<u32> for ReportId {
  fn from(val: u32) -> Self {
    ReportId(val)
  }
}
impl From<ReportId> for u32 {
  fn from(val: ReportId) -> Self {
    val.0
  }
}

/// Report count global item data type.
#[derive(Debug, Clone, Copy)]
pub struct ReportCount(u32);
impl From<&[u8]> for ReportCount {
  fn from(bytes: &[u8]) -> Self {
    ReportCount(u32_from_bytes(bytes))
  }
}
impl From<u32> for ReportCount {
  fn from(val: u32) -> Self {
    ReportCount(val)
  }
}
impl From<ReportCount> for u32 {
  fn from(val: ReportCount) -> Self {
    val.0
  }
}

/// Usage data type. This type can represent either the full 32-bit usage (if page is non-zero) or just the id (if page is zero).
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Usage(u32);
impl From<&[u8]> for Usage {
  fn from(bytes: &[u8]) -> Self {
    Usage(u32_from_bytes(bytes))
  }
}

impl Usage {
  /// Creates a Usage from the optional UsagePage and Id.
  pub fn from_page_and_id(page: Option<UsagePage>, id: Usage) -> Self {
    let mut usage = id.0;
    if let Some(page) = page {
      if usage & 0xFFFF0000 == 0 {
        usage |= (page.0 as u32) << 16;
      }
    }
    Usage(usage)
  }

  pub fn page(&self) -> u16 {
    (self.0 >> 16) as u16
  }
  pub fn id(&self) -> u16 {
    (self.0 & 0xFFFF) as u16
  }
}
impl From<u32> for Usage {
  fn from(val: u32) -> Self {
    Usage(val)
  }
}
impl From<Usage> for u32 {
  fn from(val: Usage) -> Self {
    val.0
  }
}

/// Represents a range of usages. This is computed using the USAGE_MINIMUM and USAGE_MAXIMUM local items in the descriptor, if present.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UsageRange(RangeInclusive<u32>);

impl UsageRange {
  pub fn start(&self) -> u32 {
    *self.0.start()
  }
  pub fn end(&self) -> u32 {
    *self.0.end()
  }
  pub fn range(&self) -> RangeInclusive<u32> {
    self.0.clone()
  }
  pub fn contains(&self, usage: Usage) -> bool {
    *self.0.start() <= u32::from(usage) && u32::from(usage) <= *self.0.end()
  }
}

impl From<RangeInclusive<u32>> for UsageRange {
  fn from(val: RangeInclusive<u32>) -> Self {
    UsageRange(val)
  }
}

impl From<UsageRange> for RangeInclusive<u32> {
  fn from(val: UsageRange) -> Self {
    val.0
  }
}

/// Represents an index in the Physical descriptor.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DesignatorIndex(u32);
impl From<u32> for DesignatorIndex {
  fn from(val: u32) -> Self {
    DesignatorIndex(val)
  }
}

/// Represents a range of indices in the Physical descriptor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DesignatorRange(RangeInclusive<u32>);
impl DesignatorRange {
  pub fn start(&self) -> u32 {
    *self.0.start()
  }
  pub fn range(&self) -> RangeInclusive<u32> {
    self.0.clone()
  }
}
impl From<RangeInclusive<u32>> for DesignatorRange {
  fn from(val: RangeInclusive<u32>) -> Self {
    DesignatorRange(val)
  }
}

/// Represents an index in the String descriptor.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StringIndex(u32);
impl From<u32> for StringIndex {
  fn from(val: u32) -> Self {
    StringIndex(val)
  }
}

/// Represents an range of indices in the String descriptor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringRange(RangeInclusive<u32>);
impl StringRange {
  pub fn start(&self) -> u32 {
    *self.0.start()
  }
  pub fn range(&self) -> RangeInclusive<u32> {
    self.0.clone()
  }
}
impl From<RangeInclusive<u32>> for StringRange {
  fn from(val: RangeInclusive<u32>) -> Self {
    StringRange(val)
  }
}

/// Describes the report attributes.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct ReportAttributes {
  pub constant: bool,
  pub variable: bool,
  pub relative: bool,
  pub wrap: bool,
  pub nonlinear: bool,
  pub no_preferred: bool,
  pub null_state: bool,
  pub volatile: bool,
  pub buffered_bytes: bool,
}

impl From<&[u8]> for ReportAttributes {
  fn from(data: &[u8]) -> Self {
    let attributes = u32_from_bytes(data);
    ReportAttributes {
      constant: (attributes & 0b000000001) != 0,       //0
      variable: (attributes & 0b000000010) != 0,       //1
      relative: (attributes & 0b000000100) != 0,       //2
      wrap: (attributes & 0b000001000) != 0,           //3
      nonlinear: (attributes & 0b000010000) != 0,      //4
      no_preferred: (attributes & 0b000100000) != 0,   //6
      null_state: (attributes & 0b001000000) != 0,     //6
      volatile: (attributes & 0b010000000) != 0,       //7
      buffered_bytes: (attributes & 0b100000000) != 0, //8
    }
  }
}
