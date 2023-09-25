//! HID Report Descriptor Item Tokenization Support
//!
//! This module handles tokenizing a report descriptor given as a byte slice into report descriptor item structures as
//! described in HID spec 1.1 sections 6.2.2.1 through 6.2.2.3.
//!
//! ## License
//!
//! Copyright (C) Microsoft Corporation. All rights reserved.
//!
//! SPDX-License-Identifier: BSD-2-Clause-Patent
//!

/// Identifies the type for descriptor report items. See HID spec 1.1. section 6.2.2.2.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReportItemType {
  Main,
  Global,
  Local,
  Reserved,
}

/// Parsed HID report descriptor item.
#[derive(Debug, PartialEq, Eq)]
pub struct ReportItem<'a> {
  pub item_type: ReportItemType,
  pub tag: u8,
  pub data: &'a [u8],
}

/// Item tokenizer - produces an iterator over a byte slice that returns ReportItems.
pub struct DescriptorItemTokenizer<'a> {
  descriptor: &'a [u8],
  position: usize,
}

impl<'a> DescriptorItemTokenizer<'a> {
  /// Instantiates a new HID Report Descriptor Item Tokenizer.
  pub fn new(descriptor: &'a [u8]) -> Self {
    DescriptorItemTokenizer { descriptor, position: 0 }
  }
}

impl<'a> Iterator for DescriptorItemTokenizer<'a> {
  type Item = ReportItem<'a>;
  fn next(&mut self) -> Option<Self::Item> {
    let item_header = self.descriptor.get(self.position)?;
    let mut size = item_header & 0x3;
    let item_type = match (item_header & 0xC) >> 2 {
      0 => ReportItemType::Main,
      1 => ReportItemType::Global,
      2 => ReportItemType::Local,
      3 => ReportItemType::Reserved,
      _ => unreachable!(),
    };
    let mut tag = (item_header & 0xF0) >> 4;

    if size == 3 {
      //short item size of 4 bytes is encoded as "3"
      size = 4;
    }

    self.position += 1;

    if (size == 2) && (tag == 0xF) && (item_type) == ReportItemType::Reserved {
      // long item type
      size = *self.descriptor.get(self.position)?;
      self.position += 1;
      tag = *self.descriptor.get(self.position)?;
      self.position += 1;
    }

    let data = self.descriptor.get(self.position..self.position + size as usize)?;

    self.position += size as usize;

    Some(ReportItem { item_type, tag, data })
  }
}

#[cfg(test)]
mod tests {
  use super::{DescriptorItemTokenizer, ReportItem, ReportItemType};
  use alloc::vec::Vec;

  static TEST_REPORT_DESCRIPTOR: &[u8] = &[
    // Integrated Windows Pen TLC
    0x05, 0x0d, // USAGE_PAGE (Digitizers)
    0x09, 0x02, // USAGE (Pen)
    0xa1, 0x01, // COLLECTION (Application)
    0x85, 0x01, //   REPORT_ID (Pen)
    0x09, 0x20, //   USAGE (Stylus)
    0xa1, 0x00, //   COLLECTION (Physical)
    0x09, 0x42, //     USAGE (Tip Switch)
    0x09, 0x44, //     USAGE (Barrel Switch)
    0x09, 0x3c, //     USAGE (Invert)
    0x09, 0x45, //     USAGE (Eraser Switch)
    0x15, 0x00, //     LOGICAL_MINIMUM (0)
    0x25, 0x01, //     LOGICAL_MAXIMUM (1)
    0x75, 0x01, //     REPORT_SIZE (1)
    0x95, 0x04, //     REPORT_COUNT (4)
    0x81, 0x02, //     INPUT (Data,Var,Abs)
    0x95, 0x01, //     REPORT_COUNT (1)
    0x81, 0x03, //     INPUT (Cnst,Var,Abs)
    0x09, 0x32, //     USAGE (In Range)
    0x81, 0x02, //     INPUT (Data,Var,Abs)
    0x95, 0x02, //     REPORT_COUNT (2)
    0x81, 0x03, //     INPUT (Cnst,Var,Abs)
    0x05, 0x01, //     USAGE_PAGE (Generic Desktop)
    0x09, 0x30, //     USAGE (X)
    0x75, 0x10, //     REPORT_SIZE (16)
    0x95, 0x01, //     REPORT_COUNT (1)
    0xa4, //     PUSH
    0x55, 0x0d, //     UNIT_EXPONENT (-3)
    0x65, 0x13, //     UNIT (Inch,EngLinear)
    0x35, 0x00, //     PHYSICAL_MINIMUM (0)
    0x46, 0x3a, 0x20, //     PHYSICAL_MAXIMUM (8250)
    0x26, 0xf8, 0x52, //     LOGICAL_MAXIMUM (21240)
    0x81, 0x02, //     INPUT (Data,Var,Abs)
    0x09, 0x31, //     USAGE (Y)
    0x46, 0x2c, 0x18, //     PHYSICAL_MAXIMUM (6188)
    0x26, 0x6c, 0x3e, //     LOGICAL_MAXIMUM (15980)
    0x81, 0x02, //     INPUT (Data,Var,Abs)
    0xb4, //     POP
    0x05, 0x0d, //     USAGE_PAGE (Digitizers)
    0x09, 0x30, //     USAGE (Tip Pressure)
    0x26, 0xff, 0x00, //     LOGICAL_MAXIMUM (255)
    0x81, 0x02, //     INPUT (Data,Var,Abs)
    0x75, 0x08, //     REPORT_SIZE (8)
    0x09, 0x3d, //     USAGE (X Tilt)
    0x15, 0x81, //     LOGICAL_MINIMUM (-127)
    0x25, 0x7f, //     LOGICAL_MAXIMUM (127)
    0x81, 0x02, //     INPUT (Data,Var,Abs)
    0x09, 0x3e, //     USAGE (Y Tilt)
    0x15, 0x81, //     LOGICAL_MINIMUM (-127)
    0x25, 0x7f, //     LOGICAL_MAXIMUM (127)
    0x81, 0x02, //     INPUT (Data,Var,Abs)
    0xc0, //   END_COLLECTION
    0xc0, // END_COLLECTION
  ];

  #[rustfmt::skip]
  static EXPECTED_ITEMS: &[ReportItem] = &[
    ReportItem {item_type: ReportItemType::Global, tag: 0x00, data: &[0x0d]}, // USAGE_PAGE (Digitizers)
    ReportItem {item_type: ReportItemType::Local,  tag: 0x00, data: &[0x02]}, // USAGE (Pen)
    ReportItem {item_type: ReportItemType::Main,   tag: 0x0a, data: &[0x01]}, // COLLECTION (Application)
    ReportItem {item_type: ReportItemType::Global, tag: 0x08, data: &[0x01]}, //   REPORT_ID (Pen)
    ReportItem {item_type: ReportItemType::Local,  tag: 0x00, data: &[0x20]}, //   USAGE (Stylus)
    ReportItem {item_type: ReportItemType::Main,   tag: 0x0a, data: &[0x00]}, //   COLLECTION (Physical)
    ReportItem {item_type: ReportItemType::Local,  tag: 0x00, data: &[0x42]}, //     USAGE (Tip Switch)
    ReportItem {item_type: ReportItemType::Local,  tag: 0x00, data: &[0x44]}, //     USAGE (Barrel Switch)
    ReportItem {item_type: ReportItemType::Local,  tag: 0x00, data: &[0x3c]}, //     USAGE (Invert)
    ReportItem {item_type: ReportItemType::Local,  tag: 0x00, data: &[0x45]}, //     USAGE (Eraser Switch)
    ReportItem {item_type: ReportItemType::Global, tag: 0x01, data: &[0x00]}, //     LOGICAL_MINIMUM (0)
    ReportItem {item_type: ReportItemType::Global, tag: 0x02, data: &[0x01]}, //     LOGICAL_MAXIMUM (1)
    ReportItem {item_type: ReportItemType::Global, tag: 0x07, data: &[0x01]}, //     REPORT_SIZE (1)
    ReportItem {item_type: ReportItemType::Global, tag: 0x09, data: &[0x04]}, //     REPORT_COUNT (4)
    ReportItem {item_type: ReportItemType::Main,   tag: 0x08, data: &[0x02]}, //     INPUT (Data,Var,Abs)
    ReportItem {item_type: ReportItemType::Global, tag: 0x09, data: &[0x01]}, //     REPORT_COUNT (1)
    ReportItem {item_type: ReportItemType::Main,   tag: 0x08, data: &[0x03]}, //     INPUT (Cnst,Var,Abs)
    ReportItem {item_type: ReportItemType::Local,  tag: 0x00, data: &[0x32]}, //     USAGE (In Range)
    ReportItem {item_type: ReportItemType::Main,   tag: 0x08, data: &[0x02]}, //     INPUT (Data,Var,Abs)
    ReportItem {item_type: ReportItemType::Global, tag: 0x09, data: &[0x02]}, //     REPORT_COUNT (2)
    ReportItem {item_type: ReportItemType::Main,   tag: 0x08, data: &[0x03]}, //     INPUT (Cnst,Var,Abs)
    ReportItem {item_type: ReportItemType::Global, tag: 0x00, data: &[0x01]}, //     USAGE_PAGE (Generic Desktop)
    ReportItem {item_type: ReportItemType::Local,  tag: 0x00, data: &[0x30]}, //     USAGE (X)
    ReportItem {item_type: ReportItemType::Global, tag: 0x07, data: &[0x10]}, //     REPORT_SIZE (16)
    ReportItem {item_type: ReportItemType::Global, tag: 0x09, data: &[0x01]}, //     REPORT_COUNT (1)
    ReportItem {item_type: ReportItemType::Global, tag: 0x0a, data: &[]},     //     PUSH
    ReportItem {item_type: ReportItemType::Global, tag: 0x05, data: &[0x0d]}, //     UNIT_EXPONENT (-3)
    ReportItem {item_type: ReportItemType::Global, tag: 0x06, data: &[0x13]}, //     UNIT (Inch,EngLinear)
    ReportItem {item_type: ReportItemType::Global, tag: 0x03, data: &[0x00]}, //     PHYSICAL_MINIMUM (0)
    ReportItem {item_type: ReportItemType::Global, tag: 0x04, data: &[0x3a, 0x20]}, //     PHYSICAL_MAXIMUM (8250)
    ReportItem {item_type: ReportItemType::Global, tag: 0x02, data: &[0xf8, 0x52]}, //     LOGICAL_MAXIMUM (21240)
    ReportItem {item_type: ReportItemType::Main,   tag: 0x08, data: &[0x02]}, //     INPUT (Data,Var,Abs)
    ReportItem {item_type: ReportItemType::Local,  tag: 0x00, data: &[0x31]}, //     USAGE (Y)
    ReportItem {item_type: ReportItemType::Global, tag: 0x04, data: &[0x2c, 0x18]}, //     PHYSICAL_MAXIMUM (6188)
    ReportItem {item_type: ReportItemType::Global, tag: 0x02, data: &[0x6c, 0x3e]}, //     LOGICAL_MAXIMUM (15980)
    ReportItem {item_type: ReportItemType::Main,   tag: 0x08, data: &[0x02]}, //     INPUT (Data,Var,Abs)
    ReportItem {item_type: ReportItemType::Global, tag: 0x0b, data: &[]},     //     POP
    ReportItem {item_type: ReportItemType::Global, tag: 0x00, data: &[0x0d]}, //     USAGE_PAGE (Digitizers)
    ReportItem {item_type: ReportItemType::Local,  tag: 0x00, data: &[0x30]}, //     USAGE (Tip Pressure)
    ReportItem {item_type: ReportItemType::Global, tag: 0x02, data: &[0xff, 0x00]}, //     LOGICAL_MAXIMUM (255)
    ReportItem {item_type: ReportItemType::Main,   tag: 0x08, data: &[0x02]}, //     INPUT (Data,Var,Abs)
    ReportItem {item_type: ReportItemType::Global, tag: 0x07, data: &[0x08]}, //     REPORT_SIZE (8)
    ReportItem {item_type: ReportItemType::Local,  tag: 0x00, data: &[0x3d]}, //     USAGE (X Tilt)
    ReportItem {item_type: ReportItemType::Global, tag: 0x01, data: &[0x81]}, //     LOGICAL_MINIMUM (-127)
    ReportItem {item_type: ReportItemType::Global, tag: 0x02, data: &[0x7f]}, //     LOGICAL_MAXIMUM (127)
    ReportItem {item_type: ReportItemType::Main,   tag: 0x08, data: &[0x02]}, //     INPUT (Data,Var,Abs)
    ReportItem {item_type: ReportItemType::Local,  tag: 0x00, data: &[0x3e]}, //     USAGE (Y Tilt)
    ReportItem {item_type: ReportItemType::Global, tag: 0x01, data: &[0x81]}, //     LOGICAL_MINIMUM (-127)
    ReportItem {item_type: ReportItemType::Global, tag: 0x02, data: &[0x7f]}, //     LOGICAL_MAXIMUM (127)
    ReportItem {item_type: ReportItemType::Main,   tag: 0x08, data: &[0x02]}, //     INPUT (Data,Var,Abs)
    ReportItem {item_type: ReportItemType::Main,   tag: 0x0c, data: &[]},     //   END_COLLECTION
    ReportItem {item_type: ReportItemType::Main,   tag: 0x0c, data: &[]},     // END_COLLECTION
  ];

  #[test]
  fn item_tokenizer_should_tokenize_items() {
    let tokenizer = DescriptorItemTokenizer::new(TEST_REPORT_DESCRIPTOR);

    let items: Vec<_> = tokenizer.collect();

    assert_eq!(items.len(), EXPECTED_ITEMS.len(), "tokenizer did not produce the correct number of items");

    for (index, (item, expected_item)) in items.iter().zip(EXPECTED_ITEMS.iter()).enumerate() {
      assert_eq!(item, expected_item, "invalid tokenization of item at index {index:?}");
    }
  }
}
