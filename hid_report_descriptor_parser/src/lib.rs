//! HID Report Descriptor Parser
//!
//! This crate defines data types to describe a HID report descriptor, and implements a parser that will process a raw
//! descriptor and return a ReportDescriptor object that describes the contents of the report descriptor.
//!
//! Refer to the USB Device Class Definition for Human Interface Devices (HID) Version 1.11
//! <https://www.usb.org/sites/default/files/hid1_11.pdf>
//!
//! ## Example
//! ```
//! # use hid_report_descriptor_parser::parse_report_descriptor;
//! # use hid_report_descriptor_parser::report_data_types::Usage;
//! # use hid_report_descriptor_parser::report_data_types::UsagePage;
//! # use hid_report_descriptor_parser::ReportField;
//! # use hid_report_descriptor_parser::report_data_types::UsageRange;
//!
//!   let BOOT_KEYBOARD_REPORT_DESCRIPTOR: &[u8] = &[
//!     0x05, 0x01, // USAGE_PAGE (Generic Desktop)
//!     0x09, 0x06, // USAGE (Keyboard)
//!     0xa1, 0x01, // COLLECTION (Application)
//!     0x75, 0x01, //    REPORT_SIZE (1)
//!     0x95, 0x08, //    REPORT_COUNT (8)
//!     0x05, 0x07, //    USAGE_PAGE (Key Codes)
//!     0x19, 0xE0, //    USAGE_MINIMUM (224)
//!     0x29, 0xE7, //    USAGE_MAXIMUM (231)
//!     0x15, 0x00, //    LOGICAL_MAXIMUM (0)
//!     0x25, 0x01, //    LOGICAL_MINIMUM (1)
//!     0x81, 0x02, //    INPUT (Data, Var, Abs) (Modifier Byte)
//!     0x95, 0x01, //    REPORT_COUNT (1)
//!     0x75, 0x08, //    REPORT_SIZE (8)
//!     0x81, 0x03, //    INPUT (Const) (Reserved Byte)
//!     0x95, 0x05, //    REPORT_COUNT (5)
//!     0x75, 0x01, //    REPORT_SIZE (1)
//!     0x05, 0x08, //    USAGE_PAGE (LEDs)
//!     0x19, 0x01, //    USAGE_MINIMUM (1)
//!     0x29, 0x05, //    USAGE_MAXIMUM (5)
//!     0x91, 0x02, //    OUTPUT (Data, Var, Abs) (LED report)
//!     0x95, 0x01, //    REPORT_COUNT (1)
//!     0x75, 0x03, //    REPORT_SIZE (3)
//!     0x91, 0x02, //    OUTPUT (Constant) (LED report padding)
//!     0x95, 0x06, //    REPORT_COUNT (6)
//!     0x75, 0x08, //    REPORT_SIZE (8)
//!     0x15, 0x00, //    LOGICAL_MINIMUM (0)
//!     0x26, 0xff, 00, //    LOGICAL_MAXIMUM (255)
//!     0x05, 0x07, //    USAGE_PAGE (Key Codes)
//!     0x19, 0x00, //    USAGE_MINIMUM (0)
//!     0x2a, 0xff, 00, //    USAGE_MAXIMUM (255)
//!     0x81, 0x00, //    INPUT (Data, Array)
//!     0xc0, // END_COLLECTION
//!   ];
//!
//!   let descriptor = parse_report_descriptor(BOOT_KEYBOARD_REPORT_DESCRIPTOR).unwrap();
//!   // singleton input/output reports with no report id.
//!   assert_eq!(descriptor.input_reports.len(), 1);
//!   assert_eq!(descriptor.output_reports.len(), 1);
//!   assert_eq!(descriptor.input_reports[0].report_id, None);
//!   assert_eq!(descriptor.output_reports[0].report_id, None);
//!
//!   //Input report field[4] is right control - Usage Page 7, Usage 0xE4, occupying bit 4 of the input report.
//!   let ReportField::Variable(ref field) = descriptor.input_reports[0].fields[4] else {panic!("Unexpected field type.")};
//!   let usage = Usage::from_page_and_id(Some(UsagePage::from(0x07)), Usage::from(0xe4));
//!   assert_eq!(field.usage, usage);
//!   assert_eq!(field.bits, 4..5);
//!
//!   //Input report field[8] is padding, occupying bit 8 through 15 of the input report.
//!   let ReportField::Padding(ref field) = descriptor.input_reports[0].fields[8] else {panic!("Unexpected field type.")};
//!   assert_eq!(field.bits, 8..16);
//!
//!   //Input report field[9] is the first keycode byte, occupying bit 16 through 23 of the input report.
//!   //The key code can have usages in Usage Page 7, in the range 0x00 to 0xFF
//!   let ReportField::Array(ref field) = descriptor.input_reports[0].fields[9] else {panic!("Unexpected field type.")};
//!   assert_eq!(field.bits, 16..24);
//!   assert_eq!(field.usage_list, Vec::from([UsageRange::from(0x00070000..=0x000700ff)]));
//!
//!
//!   //Output report field[1] is caps lock - Usage Page 8, Usage 0x01, occupying bit 1 of the output report.
//!   let ReportField::Variable(ref field) = descriptor.output_reports[0].fields[1] else {panic!("Unexpected field type.")};
//!   let usage = Usage::from_page_and_id(Some(UsagePage::from(0x08)), Usage::from(0x02));
//!   assert_eq!(field.usage, usage);
//!   assert_eq!(field.bits, 1..2);
//!
//! ```
//! ## License
//!
//! Copyright (C) Microsoft Corporation. All rights reserved.
//!
//! SPDX-License-Identifier: BSD-2-Clause-Patent
//!

#![no_std]
mod item_tokenizer;
pub mod report_data_types;
pub mod report_descriptor_parser;
mod utils;

extern crate alloc;
use crate::report_descriptor_parser::{ReportDescriptorError, ReportDescriptorParser};
use alloc::{vec, vec::Vec};
use core::ops::Range;
use report_data_types::{
  DesignatorIndex, DesignatorRange, LogicalMaximum, LogicalMinimum, PhysicalMaximum, PhysicalMinimum, ReportAttributes,
  ReportId, StringIndex, StringRange, Unit, UnitExponent, Usage, UsageRange,
};
use utils::u32_from_bytes;

// helper routine to extract data from report buffers as byte array.
fn field_data(bits: &Range<u32>, buffer: &[u8]) -> Option<Vec<u8>> {
  if (bits.end / 8) as usize > buffer.len() {
    return None;
  }
  let mut dst_vec = vec![0u8; (bits.len() - 1) / 8 + 1 as usize];
  for (dst_bit, src_bit) in bits.clone().enumerate() {
    let src_byte = buffer[(src_bit / 8) as usize];
    let src_bit_value = (src_byte >> (src_bit % 8)) & 0x01;
    let dst_byte_or_mask = src_bit_value << (dst_bit % 8);
    dst_vec[(dst_bit / 8) as usize] = dst_vec[(dst_bit / 8) as usize] | dst_byte_or_mask;
  }

  Some(dst_vec)
}

// helper routine to extract data from report buffer as i64
fn field_value(bits: &Range<u32>, min: LogicalMinimum, max: LogicalMaximum, buffer: &[u8]) -> Option<i64> {
  let mut field_data = field_data(bits, buffer)?;
  let mut raw_value_bytes = [0u8; 8];
  if i32::from(min).is_negative() {
    //logical minimum = negative means that the field is signed (HID 1.1 section 6.2.2.7).
    //determine whether sign-extension is required.
    let sign_position_within_msb = ((bits.len() - 1) % 8) as u8;
    let msb_idx = field_data.len() - 1;
    let msb = field_data[msb_idx];
    if msb & (1 << sign_position_within_msb) != 0 {
      //sign bit is 1, so we have to sign-extend before conversion.
      if sign_position_within_msb != 7 {
        let extended_byte = msb | !((1 << (sign_position_within_msb + 1)) - 1);
        field_data[msb_idx] = extended_byte;
      }
      raw_value_bytes.fill(0xff);
    }
  }

  raw_value_bytes[..field_data.len()].copy_from_slice(field_data.as_slice());

  let value = i64::from_le_bytes(raw_value_bytes);

  if (value < i32::from(min) as i64) || (value > i32::from(max) as i64) {
    None
  } else {
    Some(i64::from_le_bytes(raw_value_bytes))
  }
}

// helper routine to return the logical range for a field as u32
fn field_range(min: LogicalMinimum, max: LogicalMaximum) -> Option<u32> {
  if i32::from(min).is_negative() {
    //logical minimum = negative means that the field is signed.
    return i32::from(max).checked_sub(i32::from(min)).and_then(|f| if f > 0 { Some(f as u32) } else { None });
  } else {
    return u32::from(max).checked_sub(u32::from(min)).and_then(|f| if f > 0 { Some(f) } else { None });
  }
}

/// Describes a report collection.
#[derive(Debug, Clone)]
pub struct ReportCollection {
  /// The usage associated with the collection.
  pub usage: Usage,
  /// The designator index associated with the collection.
  pub designator: Option<DesignatorIndex>,
  /// The string index associated with the collection.
  pub string: Option<StringIndex>,
  /// The list of other collections that this collection is a member of.
  pub member_of: Vec<ReportCollection>,
}

/// Describes a Variable data field in a report descriptor.
#[derive(Debug, Default, Clone)]
pub struct VariableField {
  /// The bit range that the variable data field occupies in the report.
  pub bits: Range<u32>,
  /// The report attributes.
  pub attributes: ReportAttributes,
  /// The report usage. Refer to HID usage tables spec for definitions of specific usages.
  pub usage: Usage,
  /// Logical minimum value for this field.
  pub logical_minimum: LogicalMinimum,
  /// Logical maximum value for this field.
  pub logical_maximum: LogicalMaximum,
  /// Optional physical minimum value for this field.
  pub physical_minimum: Option<PhysicalMinimum>,
  /// Optional physical maximum value for this field.
  pub physical_maximum: Option<PhysicalMaximum>,
  /// Optional units definition for this field.
  pub unit: Option<Unit>,
  /// Optional unit exponent for this field.
  pub unit_exponent: Option<UnitExponent>,
  /// Optional designator index for this field.
  pub designator_index: Option<DesignatorIndex>,
  /// Optional string index for this field.
  pub string_index: Option<StringIndex>,
  /// The set of collections that this field belongs to.
  pub member_of: Vec<ReportCollection>,
}

impl VariableField {
  /// returns the data for this field from the given report buffer
  pub fn field_data(&self, buffer: &[u8]) -> Option<Vec<u8>> {
    field_data(&self.bits, buffer)
  }

  /// returns the field data as an i64 value.
  pub fn field_value(&self, buffer: &[u8]) -> Option<i64> {
    field_value(&self.bits, self.logical_minimum, self.logical_maximum, buffer)
  }

  /// returns the logical ranges of the field.
  pub fn field_range(&self) -> Option<u32> {
    field_range(self.logical_minimum, self.logical_maximum)
  }
}

/// Describes an Array data field in a report descriptor.
#[derive(Debug, Default, Clone)]
pub struct ArrayField {
  /// The bit range that the array data field occupies in the report.
  pub bits: Range<u32>,
  /// The report attributes
  pub attributes: ReportAttributes,
  /// The list of usages associated with this array field.
  pub usage_list: Vec<UsageRange>,
  /// The logical minimum for this array field.
  pub logical_minimum: LogicalMinimum,
  /// The logical maximum for this array field.
  pub logical_maximum: LogicalMaximum,
  /// The list of designators associated with this array field.
  pub designator_list: Vec<DesignatorRange>,
  //  The list of strings associated with this array field.
  pub string_list: Vec<StringRange>,
  /// The set of collections that this field belongs to.
  pub member_of: Vec<ReportCollection>,
}

impl ArrayField {
  /// returns the data for this field from the given report buffer
  pub fn field_data(&self, buffer: &[u8]) -> Option<Vec<u8>> {
    field_data(&self.bits, buffer)
  }

  /// returns the field data as an i64 value.
  pub fn field_value(&self, buffer: &[u8]) -> Option<i64> {
    field_value(&self.bits, self.logical_minimum, self.logical_maximum, buffer)
  }

  /// returns the logical ranges of the field.
  pub fn field_range(&self) -> Option<u32> {
    field_range(self.logical_minimum, self.logical_maximum)
  }
}

/// Describes a Padding data field in a report descriptor (i.e. a field without usages).
#[derive(Debug, Clone)]
pub struct PaddingField {
  /// The bit range that the padding data field occupies in the report.
  pub bits: Range<u32>,
}

/// Defines the types of fields that appear in a report.
#[derive(Debug, Clone)]
pub enum ReportField {
  Variable(VariableField),
  Array(ArrayField),
  Padding(PaddingField),
}

/// Describes a report.
#[derive(Debug)]
pub struct Report {
  /// The (optional) report id associated with the report.
  pub report_id: Option<ReportId>,
  /// The size in bits of the report.
  pub size_in_bits: usize,
  /// The list of fields in the report.
  pub fields: Vec<ReportField>,
}

/// A collection of input/output/feature reports that are described by a given Report Descriptor.
pub struct ReportDescriptor {
  /// The list of input reports from this report descriptor.
  pub input_reports: Vec<Report>,
  /// The list of output reports from this report descriptor.
  pub output_reports: Vec<Report>,
  /// The list of feature reports from this report descriptor.
  pub features: Vec<Report>,
}

/// Parse the raw report descriptor in the given byte slice.
pub fn parse_report_descriptor(report_descriptor: &[u8]) -> Result<ReportDescriptor, ReportDescriptorError> {
  ReportDescriptorParser::parse(report_descriptor)
}

#[cfg(test)]
mod tests {
  extern crate std;

  use crate::{
    report_data_types::{LogicalMaximum, LogicalMinimum},
    ArrayField, VariableField,
  };
  use alloc::vec;

  #[test]
  fn field_data_should_return_field_data() {
    let mut field: VariableField = Default::default();
    let buffer: [u8; 10] = [0xaa; 10];

    field.bits = 0..8;
    assert_eq!(field.field_data(&buffer), Some(vec![0xaa]));

    field.bits = 1..5;
    assert_eq!(field.field_data(&buffer), Some(vec![0x5]));

    field.bits = 3..9;
    assert_eq!(field.field_data(&buffer), Some(vec![0x15]));

    field.bits = 7..21;
    assert_eq!(field.field_data(&buffer), Some(vec![0x55, 0x15]));

    field.bits = 100..257;
    assert_eq!(field.field_data(&buffer), None);

    let mut field: ArrayField = Default::default();
    let buffer: [u8; 10] = [0x71; 10];

    field.bits = 0..8;
    assert_eq!(field.field_data(&buffer), Some(vec![0x71]));

    field.bits = 1..5;

    assert_eq!(field.field_data(&buffer), Some(vec![0x8]));

    field.bits = 3..9;
    assert_eq!(field.field_data(&buffer), Some(vec![0x2E]));

    field.bits = 7..21;
    assert_eq!(field.field_data(&buffer), Some(vec![0xE2, 0x22]));

    field.bits = 100..257;
    assert_eq!(field.field_data(&buffer), None);
  }

  #[test]
  fn field_value_should_return_field_value() {
    let mut field: VariableField = Default::default();

    field.logical_minimum = LogicalMinimum::from(-127i32);
    field.logical_maximum = LogicalMaximum::from(127i32);
    field.bits = 0..8;

    let buffer = (-1i8).to_le_bytes();
    assert_eq!(field.field_value(&buffer), Some(-1i64));

    let buffer = (-17i8).to_le_bytes();
    assert_eq!(field.field_value(&buffer), Some(-17i64));

    let buffer = (127i8).to_le_bytes();
    assert_eq!(field.field_value(&buffer), Some(127i64));

    field.logical_minimum = LogicalMinimum::from(-1024i32);
    field.logical_maximum = LogicalMaximum::from(1024i32);
    field.bits = 0..11;

    let buffer = (-1i16).to_le_bytes();
    assert_eq!(field.field_value(&buffer), Some(-1i64));

    let buffer = (-17i16).to_le_bytes();
    assert_eq!(field.field_value(&buffer), Some(-17i64));

    let buffer = (127i16).to_le_bytes();
    assert_eq!(field.field_value(&buffer), Some(127i64));

    field.logical_minimum = LogicalMinimum::from(-1024i32);
    field.logical_maximum = LogicalMaximum::from(1024i32);
    field.bits = 1..12;

    let buffer = (-1i16).to_le_bytes();
    assert_eq!(field.field_value(&buffer), Some(-1i64));

    let buffer = (-18i16).to_le_bytes();
    assert_eq!(field.field_value(&buffer), Some(-9i64));

    let buffer = (128i16).to_le_bytes();
    assert_eq!(field.field_value(&buffer), Some(64i64));

    field.logical_minimum = LogicalMinimum::from(-1024i32);
    field.logical_maximum = LogicalMaximum::from(1024i32);
    field.bits = 0..12;

    let buffer = (1025i16).to_le_bytes();
    assert_eq!(field.field_value(&buffer), None);

    let buffer = (-1025i16).to_le_bytes();
    assert_eq!(field.field_value(&buffer), None);

    let mut field: ArrayField = Default::default();

    field.logical_minimum = LogicalMinimum::from(-127i32);
    field.logical_maximum = LogicalMaximum::from(127i32);
    field.bits = 0..8;

    let buffer = (-1i8).to_le_bytes();
    assert_eq!(field.field_value(&buffer), Some(-1i64));

    let buffer = (-17i8).to_le_bytes();
    assert_eq!(field.field_value(&buffer), Some(-17i64));

    let buffer = (127i8).to_le_bytes();
    assert_eq!(field.field_value(&buffer), Some(127i64));

    field.logical_minimum = LogicalMinimum::from(-1024i32);
    field.logical_maximum = LogicalMaximum::from(1024i32);
    field.bits = 0..11;

    let buffer = (-1i16).to_le_bytes();
    assert_eq!(field.field_value(&buffer), Some(-1i64));

    let buffer = (-17i16).to_le_bytes();
    assert_eq!(field.field_value(&buffer), Some(-17i64));

    let buffer = (127i16).to_le_bytes();
    assert_eq!(field.field_value(&buffer), Some(127i64));

    field.logical_minimum = LogicalMinimum::from(-1024i32);
    field.logical_maximum = LogicalMaximum::from(1024i32);
    field.bits = 1..12;

    let buffer = (-1i16).to_le_bytes();
    assert_eq!(field.field_value(&buffer), Some(-1i64));

    let buffer = (-18i16).to_le_bytes();
    assert_eq!(field.field_value(&buffer), Some(-9i64));

    let buffer = (128i16).to_le_bytes();
    assert_eq!(field.field_value(&buffer), Some(64i64));

    field.logical_minimum = LogicalMinimum::from(-1024i32);
    field.logical_maximum = LogicalMaximum::from(1024i32);
    field.bits = 0..12;

    let buffer = (1025i16).to_le_bytes();
    assert_eq!(field.field_value(&buffer), None);

    let buffer = (-1025i16).to_le_bytes();
    assert_eq!(field.field_value(&buffer), None);
  }

  #[test]
  fn field_range_should_return_field_range() {
    let mut field: VariableField = Default::default();

    field.logical_minimum = LogicalMinimum::from(-127i32);
    field.logical_maximum = LogicalMaximum::from(127i32);

    assert_eq!(field.field_range(), Some(254u32));

    field.logical_minimum = LogicalMinimum::from(-127i32);
    field.logical_maximum = LogicalMaximum::from(-120i32);

    assert_eq!(field.field_range(), Some(7u32));

    field.logical_minimum = LogicalMinimum::from(128i32);
    field.logical_maximum = LogicalMaximum::from(384i32);

    assert_eq!(field.field_range(), Some(256u32));

    field.logical_minimum = LogicalMinimum::from(0i32);
    field.logical_maximum = LogicalMaximum::from(u32::MAX);

    assert_eq!(field.field_range(), Some(u32::MAX));

    field.logical_minimum = LogicalMinimum::from(0i32);
    field.logical_maximum = LogicalMaximum::from(0u32);

    assert_eq!(field.field_range(), None);

    field.logical_minimum = LogicalMinimum::from(-127i32);
    field.logical_maximum = LogicalMaximum::from(-127i32);

    assert_eq!(field.field_range(), None);

    field.logical_minimum = LogicalMinimum::from(-1i32);
    field.logical_maximum = LogicalMaximum::from(-127i32);

    assert_eq!(field.field_range(), None);

    field.logical_minimum = LogicalMinimum::from(i32::MAX);
    field.logical_maximum = LogicalMaximum::from(0);

    assert_eq!(field.field_range(), None);

    // tricky: abs(i32::MIN) > i32:MAX
    field.logical_minimum = LogicalMinimum::from(i32::MIN + 1);
    field.logical_maximum = LogicalMaximum::from(0);

    assert_eq!(field.field_range(), Some(i32::MAX as u32));

    let mut field: ArrayField = Default::default();

    field.logical_minimum = LogicalMinimum::from(-127i32);
    field.logical_maximum = LogicalMaximum::from(127i32);

    assert_eq!(field.field_range(), Some(254u32));

    field.logical_minimum = LogicalMinimum::from(-127i32);
    field.logical_maximum = LogicalMaximum::from(-120i32);

    assert_eq!(field.field_range(), Some(7u32));

    field.logical_minimum = LogicalMinimum::from(128i32);
    field.logical_maximum = LogicalMaximum::from(384i32);

    assert_eq!(field.field_range(), Some(256u32));

    field.logical_minimum = LogicalMinimum::from(0i32);
    field.logical_maximum = LogicalMaximum::from(u32::MAX);

    assert_eq!(field.field_range(), Some(u32::MAX));

    field.logical_minimum = LogicalMinimum::from(0i32);
    field.logical_maximum = LogicalMaximum::from(0u32);

    assert_eq!(field.field_range(), None);

    field.logical_minimum = LogicalMinimum::from(-127i32);
    field.logical_maximum = LogicalMaximum::from(-127i32);

    assert_eq!(field.field_range(), None);

    field.logical_minimum = LogicalMinimum::from(-1i32);
    field.logical_maximum = LogicalMaximum::from(-127i32);

    assert_eq!(field.field_range(), None);

    field.logical_minimum = LogicalMinimum::from(i32::MAX);
    field.logical_maximum = LogicalMaximum::from(0);

    assert_eq!(field.field_range(), None);

    // tricky: abs(i32::MIN) > i32:MAX
    field.logical_minimum = LogicalMinimum::from(i32::MIN + 1);
    field.logical_maximum = LogicalMaximum::from(0);

    assert_eq!(field.field_range(), Some(i32::MAX as u32));
  }
}
