//! Report Descriptor Parser Core
//!
//! This module handles parsing a report descriptor given as a byte slice. After parsing, the parser can be queried to
//! retrieve the list of input, output, and feature reports that were contained in the report descriptor.
//!
//! ## License
//!
//! Copyright (C) Microsoft Corporation. All rights reserved.
//!
//! SPDX-License-Identifier: BSD-2-Clause-Patent
//!
use alloc::{collections::BTreeMap, vec, vec::Vec};

use crate::{
  item_tokenizer::{DescriptorItemTokenizer, ReportItem, ReportItemType},
  report_data_types::{ReportCount, ReportSize, UsagePage},
  u32_from_bytes, ArrayField, DesignatorIndex, DesignatorRange, LogicalMaximum, LogicalMinimum, PaddingField,
  PhysicalMaximum, PhysicalMinimum, Report, ReportAttributes, ReportCollection, ReportDescriptor, ReportField,
  ReportId, StringIndex, StringRange, Unit, UnitExponent, Usage, UsageRange, VariableField,
};

/// Defines errors generated during report descriptor parsing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReportDescriptorError {
  InvalidMainItem,
  InvalidGlobalItem,
  InvalidLocalItem,
  ReservedItemNotSupported,
  InvalidPop,
  DelimiterNotSupported,
  InvalidReportNoSize,
  InvalidReportNoCount,
  InvalidReportNoUsage,
  InvalidReportNoLogicalMin,
  InvalidReportNoLogicalMax,
  InvalidReportLogicalRange,
}

// Tracks Global State as parsing proceeds.
// A snapshot of this structure is included with each ReportData that is produced by parsing.
#[derive(Debug, Clone, Default)]
struct GlobalItemStateTable {
  usage_page: Option<UsagePage>,
  logical_minimum: Option<LogicalMinimum>,
  logical_maximum: Option<LogicalMaximum>,
  physical_minimum: Option<PhysicalMinimum>,
  physical_maximum: Option<PhysicalMaximum>,
  unit_exponent: Option<UnitExponent>,
  unit: Option<Unit>,
  report_size: Option<ReportSize>,
  report_id: Option<ReportId>,
  report_count: Option<ReportCount>,
}

// Tracks Local State as parsing proceeds.
// A snapshot of this structure is included with each ReportData that is produced by parsing.
// This structure is reset to defaults after each ReportData is generated.
#[derive(Debug, Clone, Default)]
struct LocalItemStateTable {
  usages: Vec<UsageRange>,
  usage_minimum: Option<u32>,
  usage_maximum: Option<u32>,
  designators: Vec<DesignatorRange>,
  designator_minimum: Option<u32>,
  designator_maximum: Option<u32>,
  strings: Vec<StringRange>,
  string_minimum: Option<u32>,
  string_maximum: Option<u32>,
}

// Represents a single ReportData. This is generated for each input/output/feature item.
struct ReportData {
  attributes: ReportAttributes,
  global_state: GlobalItemStateTable,
  local_state: LocalItemStateTable,
  member_of: Vec<ReportCollection>,
}

impl ReportData {
  fn new(
    item_data: &[u8],
    global_state: &GlobalItemStateTable,
    local_state: &LocalItemStateTable,
    active_collections: &Vec<ReportCollection>,
  ) -> Self {
    ReportData {
      attributes: ReportAttributes::from(item_data),
      global_state: global_state.clone(),
      local_state: local_state.clone(),
      member_of: active_collections.clone(),
    }
  }
}

/// Parses Report Descriptors and produces descriptions of the reports they contain.
pub struct ReportDescriptorParser {
  global_state: Vec<GlobalItemStateTable>,
  local_state: LocalItemStateTable,
  active_collections: Vec<ReportCollection>,

  input_reports: BTreeMap<Option<ReportId>, Vec<ReportData>>,
  output_reports: BTreeMap<Option<ReportId>, Vec<ReportData>>,
  features: BTreeMap<Option<ReportId>, Vec<ReportData>>,
}

impl ReportDescriptorParser {
  // Instantiates a new report descriptor parser.
  fn new() -> Self {
    ReportDescriptorParser {
      global_state: vec![Default::default()],
      local_state: Default::default(),
      active_collections: Vec::new(),
      input_reports: BTreeMap::new(),
      output_reports: BTreeMap::new(),
      features: BTreeMap::new(),
    }
  }

  // handles parsing for "main" items (Input/Output/Feature/Collection)
  fn parse_main(&mut self, item: ReportItem) -> Result<(), ReportDescriptorError> {
    let report_id = self.global_state[0].report_id;
    match item.tag {
      0b1000 => {
        //Input
        let mut input_vec = match self.input_reports.remove(&report_id) {
          Some(vec) => vec,
          None => Vec::new(),
        };
        input_vec.push(ReportData::new(item.data, &self.global_state[0], &self.local_state, &self.active_collections));
        self.input_reports.insert(report_id, input_vec);
      }
      0b1001 => {
        //Output
        let mut output_vec = match self.output_reports.remove(&report_id) {
          Some(vec) => vec,
          None => Vec::new(),
        };
        output_vec.push(ReportData::new(item.data, &self.global_state[0], &self.local_state, &self.active_collections));
        self.output_reports.insert(report_id, output_vec);
      }
      0b1011 => {
        //Feature
        let mut feature_vec = match self.features.remove(&report_id) {
          Some(vec) => vec,
          None => Vec::new(),
        };
        feature_vec.push(ReportData::new(
          item.data,
          &self.global_state[0],
          &self.local_state,
          &self.active_collections,
        ));
        self.features.insert(report_id, feature_vec);
      }
      0b1010 => {
        //Collection
        let usage_page = (*self).global_state[0].usage_page;
        let usage_range = (*self).local_state.usages.get(0);
        let usage_range_start = match usage_range {
          Some(&ref range) => range.start(),
          None => 0,
        };
        let usage = Usage::from_page_and_id(usage_page, Usage::from(usage_range_start));
        let designator = self.local_state.designators.get(0).clone().map(|x| DesignatorIndex::from(x.start()));
        let string = self.local_state.strings.get(0).clone().map(|x| StringIndex::from(x.start()));
        let collection = ReportCollection {
          usage: usage,
          designator: designator,
          string: string,
          member_of: self.active_collections.clone(),
        };
        self.active_collections.push(collection);
      }
      0b1100 => {
        //End Collection
        self.active_collections.pop();
      }
      _ => return Err(ReportDescriptorError::InvalidMainItem),
    }
    //reset local state after processing a main item.
    self.local_state = Default::default();
    Ok(())
  }

  // handles parsing for "global" items
  fn parse_global(&mut self, item: ReportItem) -> Result<(), ReportDescriptorError> {
    match item.tag {
      0b0000 => self.global_state[0].usage_page = Some(UsagePage::from(item.data)),
      0b0001 => self.global_state[0].logical_minimum = Some(LogicalMinimum::from(item.data)),
      0b0010 => self.global_state[0].logical_maximum = Some(LogicalMaximum::from(item.data)),
      0b0011 => self.global_state[0].physical_minimum = Some(PhysicalMinimum::from(item.data)),
      0b0100 => self.global_state[0].physical_maximum = Some(PhysicalMaximum::from(item.data)),
      0b0101 => self.global_state[0].unit_exponent = Some(UnitExponent::from(item.data)),
      0b0110 => self.global_state[0].unit = Some(Unit::from(item.data)),
      0b0111 => self.global_state[0].report_size = Some(ReportSize::from(item.data)),
      0b1000 => self.global_state[0].report_id = Some(ReportId::from(item.data)),
      0b1001 => self.global_state[0].report_count = Some(ReportCount::from(item.data)),
      0b1010 => self.global_state.push(self.global_state[0].clone()),
      0b1011 => {
        if self.global_state.len() < 2 {
          // have to have at least 2 global states to pop.
          return Err(ReportDescriptorError::InvalidPop);
        }
        self.global_state.pop().ok_or(ReportDescriptorError::InvalidPop)?;
      }
      _ => return Err(ReportDescriptorError::InvalidGlobalItem),
    };
    Ok(())
  }

  // handles parsing for "local" items
  fn parse_local(&mut self, item: ReportItem) -> Result<(), ReportDescriptorError> {
    match item.tag {
      0b0000 => {
        //Usage
        let usage = u32_from_bytes(item.data);
        self.local_state.usages.push(UsageRange::from(usage..=usage));
      }
      0b0001 => {
        //Usage Minimum
        let min = u32_from_bytes(item.data);
        if let Some(max) = self.local_state.usage_maximum.take() {
          self.local_state.usages.push(UsageRange::from(min..=max));
        } else {
          self.local_state.usage_minimum = Some(min);
        }
      }
      0b0010 => {
        //Usage Maximum
        let max = u32_from_bytes(item.data);
        if let Some(min) = self.local_state.usage_minimum.take() {
          self.local_state.usages.push(UsageRange::from(min..=max));
        } else {
          self.local_state.usage_maximum = Some(max);
        }
      }
      0b0011 => {
        //Designator Index
        let designator = u32_from_bytes(item.data);
        self.local_state.designators.push(DesignatorRange::from(designator..=designator));
      }
      0b0100 => {
        //Designator Minimum
        let min = u32_from_bytes(item.data);
        if let Some(max) = self.local_state.designator_maximum.take() {
          self.local_state.designators.push(DesignatorRange::from(min..=max));
        } else {
          self.local_state.designator_minimum = Some(min);
        }
      }
      0b0101 => {
        //Designator Maximum
        let max = u32_from_bytes(item.data);
        if let Some(min) = self.local_state.designator_minimum.take() {
          self.local_state.designators.push(DesignatorRange::from(min..=max));
        } else {
          self.local_state.designator_maximum = Some(max);
        }
      }
      0b0111 => {
        //String Index
        let string_idx = u32_from_bytes(item.data);
        self.local_state.strings.push(StringRange::from(string_idx..=string_idx));
      }
      0b1000 => {
        //String Minimum
        let min = u32_from_bytes(item.data);
        if let Some(max) = self.local_state.string_maximum.take() {
          self.local_state.strings.push(StringRange::from(min..=max));
        } else {
          self.local_state.string_minimum = Some(min);
        }
      }
      0b1001 => {
        let max = u32_from_bytes(item.data);
        if let Some(min) = self.local_state.string_minimum.take() {
          self.local_state.strings.push(StringRange::from(min..=max));
        } else {
          self.local_state.string_maximum = Some(max);
        }
      }
      0b1010 => return Err(ReportDescriptorError::DelimiterNotSupported), //Delimiter not yet supported.
      _ => return Err(ReportDescriptorError::InvalidLocalItem),
    }
    Ok(())
  }

  // Processes the given item in the parser and updates parser state.
  // Items that are malformed or unsupported will result in a ReportDescriptorError, and the state
  // of the parser will be unaffected.
  fn parse_item(&mut self, item: ReportItem) -> Result<(), ReportDescriptorError> {
    match item.item_type {
      ReportItemType::Main => self.parse_main(item),
      ReportItemType::Global => self.parse_global(item),
      ReportItemType::Local => self.parse_local(item),
      ReportItemType::Reserved => Err(ReportDescriptorError::ReservedItemNotSupported),
    }
  }

  // Collate disparate ReportData elements into full Report objects. This is invoked after parsing is completed to assemble
  // the collected ReportData elements into full reports keyed to report ids.
  fn process_reports(
    &self,
    report_records: &BTreeMap<Option<ReportId>, Vec<ReportData>>,
  ) -> Result<Vec<Report>, ReportDescriptorError> {
    let mut reports = Vec::new();

    for (id, report_data) in report_records {
      let mut fields = Vec::new();
      let mut bit_position: u32 = 0;
      for data in report_data {
        let report_count = data.global_state.report_count.ok_or(ReportDescriptorError::InvalidReportNoCount)?.into();
        let report_size: u32 = data.global_state.report_size.ok_or(ReportDescriptorError::InvalidReportNoSize)?.into();

        if data.local_state.usages.len() == 0 {
          //no usages defined - padding.
          let padding_size = report_count * report_size;
          let bits = bit_position..(bit_position + padding_size);
          bit_position += padding_size;
          fields.push(ReportField::Padding(PaddingField { bits }));
          continue;
        }

        // validate state
        let logical_min = data.global_state.logical_minimum.ok_or(ReportDescriptorError::InvalidReportNoLogicalMin)?;
        let logical_max = data.global_state.logical_maximum.ok_or(ReportDescriptorError::InvalidReportNoLogicalMax)?;

        // if logical_min is negative, then logical max is signed (i32), otherwise it is unsigned (u32).
        if i32::from(logical_min).is_negative() {
          if i32::from(logical_min) >= i32::from(logical_max) {
            Err(ReportDescriptorError::InvalidReportLogicalRange)?;
          }
        } else {
          if u32::from(logical_min) >= u32::from(logical_max) {
            Err(ReportDescriptorError::InvalidReportLogicalRange)?;
          }
        }

        if data.attributes.variable {
          // Process variable-type fields.
          let mut usage_iterator =
            data.local_state.usages.iter().map(|x| x.range()).flatten().map(|x| Usage::from(x)).peekable();

          let mut designator_iterator = data
            .local_state
            .designators
            .iter()
            .map(|x| x.range())
            .flatten()
            .map(|x| DesignatorIndex::from(x))
            .peekable();

          let mut string_iterator =
            data.local_state.strings.iter().map(|x| x.range()).flatten().map(|x| StringIndex::from(x)).peekable();

          let mut usage = usage_iterator.next().ok_or(ReportDescriptorError::InvalidReportNoUsage)?;
          let mut designator = designator_iterator.next();
          let mut string_index = string_iterator.next();

          for _ in 0..report_count {
            let bits = bit_position..(bit_position + report_size);
            bit_position += report_size;

            let field = VariableField {
              attributes: data.attributes,
              bits: bits,
              usage: Usage::from_page_and_id(data.global_state.usage_page, usage),
              logical_minimum: logical_min,
              logical_maximum: logical_max,
              physical_minimum: data.global_state.physical_minimum,
              physical_maximum: data.global_state.physical_maximum,
              unit_exponent: data.global_state.unit_exponent,
              unit: data.global_state.unit,
              designator_index: designator,
              string_index: string_index,
              member_of: data.member_of.clone(),
            };
            fields.push(ReportField::Variable(field));

            if usage_iterator.peek().is_some() {
              usage = usage_iterator.next().unwrap();
            }
            if designator_iterator.peek().is_some() {
              designator = designator_iterator.next();
            }
            if string_iterator.peek().is_some() {
              string_index = string_iterator.next();
            }
          }
        } else {
          //Process array-type fields
          for _ in 0..report_count {
            let bits = bit_position..(bit_position + report_size);
            bit_position += report_size;

            //add global usage page to usage ranges.
            let usage_list = data
              .local_state
              .usages
              .clone()
              .iter()
              .map(|usage_range| {
                let start = Usage::from(usage_range.start());
                let end = Usage::from(usage_range.end());
                let start = Usage::from_page_and_id(data.global_state.usage_page, start);
                let end = Usage::from_page_and_id(data.global_state.usage_page, end);
                UsageRange::from(start.into()..=end.into())
              })
              .collect();

            let field = ArrayField {
              attributes: data.attributes,
              bits: bits,
              usage_list: usage_list,
              logical_minimum: logical_min,
              logical_maximum: logical_max,
              designator_list: data.local_state.designators.clone(),
              string_list: data.local_state.strings.clone(),
              member_of: data.member_of.clone(),
            };
            fields.push(ReportField::Array(field));
          }
        }
      }

      reports.push(Report { report_id: id.clone(), size_in_bits: bit_position as usize, fields: fields });
    }

    Ok(reports)
  }

  /// Parses the given report_descriptor byte slice and produces a ReportDescriptor structure that describes the reports
  /// defined by the ReportDescriptor, or an Error if the ReportDescriptor cannot be parsed.
  pub fn parse(report_descriptor: &[u8]) -> Result<ReportDescriptor, ReportDescriptorError> {
    let item_tokenizer = DescriptorItemTokenizer::new(report_descriptor);
    let mut parser = Self::new();
    for item in item_tokenizer {
      parser.parse_item(item)?;
    }
    Ok(ReportDescriptor {
      input_reports: parser.process_reports(&parser.input_reports)?,
      output_reports: parser.process_reports(&parser.output_reports)?,
      features: parser.process_reports(&parser.features)?,
    })
  }
}

#[cfg(test)]
mod tests {
  use alloc::vec::Vec;

  use crate::{
    report_data_types::{
      LogicalMaximum, LogicalMinimum, PhysicalMaximum, PhysicalMinimum, ReportAttributes, ReportId, Unit, UnitExponent,
      Usage, UsagePage, UsageRange,
    },
    report_descriptor_parser::ReportDescriptorError,
    ReportField,
  };

  use super::ReportDescriptorParser;

  //Sample descriptor from:
  //https://learn.microsoft.com/en-us/windows-hardware/design/component-guidelines/touchscreen-sample-report-descriptors
  //NOTE: this descriptor does not reset global state (e.g. units) in places where it would be intuitive to do so; so
  //things like "vendor defined" fields at the end inherit global state like units/physical extent. The tests below
  //account for this, even though it is unlikely that the author of this descriptor intends that "vendor defined fields"
  //would have physical extents or units.
  static DIGITIZER_REPORT_DESCRIPTOR: &[u8] = &[
    0x05, 0x0d, // USAGE_PAGE (Digitizers)
    0x09, 0x04, // USAGE (Touch Screen)
    0xa1, 0x01, // COLLECTION (Application)
    0x85, 0x01, //   REPORT_ID (Touch)
    0x09, 0x22, //   USAGE (Finger)
    0xa1, 0x02, //     COLLECTION (Logical)
    0x09, 0x42, //       USAGE (Tip Switch)
    0x15, 0x00, //       LOGICAL_MINIMUM (0)
    0x25, 0x01, //       LOGICAL_MAXIMUM (1)
    0x75, 0x01, //       REPORT_SIZE (1)
    0x95, 0x01, //       REPORT_COUNT (1)
    0x81, 0x02, //       INPUT (Data,Var,Abs)
    0x95, 0x07, //       REPORT_COUNT (7)
    0x81, 0x03, //       INPUT (Cnst,Ary,Abs)
    0x75, 0x08, //       REPORT_SIZE (8)
    0x09, 0x51, //       USAGE (Contact Identifier)
    0x95, 0x01, //       REPORT_COUNT (1)
    0x81, 0x02, //       INPUT (Data,Var,Abs)
    0x05, 0x01, //       USAGE_PAGE (Generic Desktop)
    0x26, 0xff, 0x0f, //       LOGICAL_MAXIMUM (4095)
    0x75, 0x10, //       REPORT_SIZE (16)
    0x55, 0x0e, //       UNIT_EXPONENT (-2)
    0x65, 0x13, //       Unit::from(Inch,EngLinear)
    0x09, 0x30, //       USAGE (X)
    0x35, 0x00, //       PHYSICAL_MINIMUM (0)
    0x46, 0xb5, 0x04, //       PHYSICAL_MAXIMUM (1205)
    0x95, 0x02, //       REPORT_COUNT (2)
    0x81, 0x02, //       INPUT (Data,Var,Abs)
    0x46, 0x8a, 0x03, //       PHYSICAL_MAXIMUM (906)
    0x09, 0x31, //       USAGE (Y)
    0x81, 0x02, //       INPUT (Data,Var,Abs)
    0x05, 0x0d, //       USAGE_PAGE (Digitizers)
    0x09, 0x48, //       USAGE (Width)
    0x09, 0x49, //       USAGE (Height)
    0x81, 0x02, //       INPUT (Data,Var,Abs)
    0x95, 0x01, //       REPORT_COUNT (1)
    0x55, 0x0C, //       UNIT_EXPONENT (-4)
    0x65, 0x12, //       UNIT (Radians,SI Rotation)
    0x35, 0x00, //       PHYSICAL_MINIMUM (0)
    0x47, 0x6f, 0xf5, 0x00, 0x00, //       PHYSICAL_MAXIMUM (62831)
    0x15, 0x00, //       LOGICAL_MINIMUM (0)
    0x27, 0x6f, 0xf5, 0x00, 0x00, //       LOGICAL_MAXIMUM (62831)
    0x09, 0x3f, //       USAGE (Azimuth[Orientation])
    0x81, 0x02, //       INPUT (Data,Var,Abs)
    0xc0, //     END_COLLECTION
    0x09, 0x22, //   USAGE (Finger)
    0xa1, 0x02, //     COLLECTION (Logical)
    0x09, 0x42, //       USAGE (Tip Switch)
    0x15, 0x00, //       LOGICAL_MINIMUM (0)
    0x25, 0x01, //       LOGICAL_MAXIMUM (1)
    0x75, 0x01, //       REPORT_SIZE (1)
    0x95, 0x01, //       REPORT_COUNT (1)
    0x81, 0x02, //       INPUT (Data,Var,Abs)
    0x95, 0x07, //       REPORT_COUNT (7)
    0x81, 0x03, //       INPUT (Cnst,Ary,Abs)
    0x75, 0x08, //       REPORT_SIZE (8)
    0x09, 0x51, //       USAGE (Contact Identifier)
    0x95, 0x01, //       REPORT_COUNT (1)
    0x81, 0x02, //       INPUT (Data,Var,Abs)
    0x05, 0x01, //       USAGE_PAGE (Generic Desk..
    0x26, 0xff, 0x0f, //       LOGICAL_MAXIMUM (4095)
    0x75, 0x10, //       REPORT_SIZE (16)
    0x55, 0x0e, //       UNIT_EXPONENT (-2)
    0x65, 0x13, //       Unit::from(Inch,EngLinear)
    0x09, 0x30, //       USAGE (X)
    0x35, 0x00, //       PHYSICAL_MINIMUM (0)
    0x46, 0xb5, 0x04, //       PHYSICAL_MAXIMUM (1205)
    0x95, 0x02, //       REPORT_COUNT (2)
    0x81, 0x02, //       INPUT (Data,Var,Abs)
    0x46, 0x8a, 0x03, //       PHYSICAL_MAXIMUM (906)
    0x09, 0x31, //       USAGE (Y)
    0x81, 0x02, //       INPUT (Data,Var,Abs)
    0x05, 0x0d, //       USAGE_PAGE (Digitizers)
    0x09, 0x48, //       USAGE (Width)
    0x09, 0x49, //       USAGE (Height)
    0x81, 0x02, //       INPUT (Data,Var,Abs)
    0x95, 0x01, //       REPORT_COUNT (1)
    0x55, 0x0C, //       UNIT_EXPONENT (-4)
    0x65, 0x12, //       UNIT (Radians,SI Rotation)
    0x35, 0x00, //       PHYSICAL_MINIMUM (0)
    0x47, 0x6f, 0xf5, 0x00, 0x00, //       PHYSICAL_MAXIMUM (62831)
    0x15, 0x00, //       LOGICAL_MINIMUM (0)
    0x27, 0x6f, 0xf5, 0x00, 0x00, //       LOGICAL_MAXIMUM (62831)
    0x09, 0x3f, //       USAGE (Azimuth[Orientation])
    0x81, 0x02, //       INPUT (Data,Var,Abs)
    0xc0, //     END_COLLECTION
    0x05, 0x0d, //   USAGE_PAGE (Digitizers)
    0x55, 0x0C, //     UNIT_EXPONENT (-4)
    0x66, 0x01, 0x10, //     UNIT (Seconds)
    0x47, 0xff, 0xff, 0x00, 0x00, //       PHYSICAL_MAXIMUM (65535)
    0x27, 0xff, 0xff, 0x00, 0x00, //   LOGICAL_MAXIMUM (65535)
    0x75, 0x10, //   REPORT_SIZE (16)
    0x95, 0x01, //   REPORT_COUNT (1)
    0x09, 0x56, //   USAGE (Scan Time)
    0x81, 0x02, //   INPUT (Data,Var,Abs)
    0x09, 0x54, //   USAGE (Contact count)
    0x25, 0x7f, //   LOGICAL_MAXIMUM (127)
    0x95, 0x01, //   REPORT_COUNT (1)
    0x75, 0x08, //   REPORT_SIZE (8)
    0x81, 0x02, //   INPUT (Data,Var,Abs)
    0x85, 0x02, //   REPORT_ID (Feature)
    0x09, 0x55, //   Usage::from(Contact Count Maximum)
    0x95, 0x01, //   REPORT_COUNT (1)
    0x25, 0x02, //   LOGICAL_MAXIMUM (2)
    0xb1, 0x02, //   FEATURE (Data,Var,Abs)
    0x85, 0x44, //   REPORT_ID (Feature)
    0x06, 0x00, 0xff, //   USAGE_PAGE (Vendor Defined)
    0x09, 0xC5, //   USAGE (Vendor Usage 0xC5)
    0x15, 0x00, //   LOGICAL_MINIMUM (0)
    0x26, 0xff, 0x00, //   LOGICAL_MAXIMUM (0xff)
    0x75, 0x08, //   REPORT_SIZE (8)
    0x96, 0x00, 0x01, //   REPORT_COUNT (0x100 (256))
    0xb1, 0x02, //   FEATURE (Data,Var,Abs)
    0xc0, // END_COLLECTION
  ];

  #[test]
  fn test_digitizer_report_descriptor() {
    let report_descriptor = ReportDescriptorParser::parse(DIGITIZER_REPORT_DESCRIPTOR).unwrap();

    // Validate Input Reports
    assert_eq!(report_descriptor.input_reports.len(), 1);
    let input_report = &report_descriptor.input_reports[0];

    assert_eq!(input_report.fields.len(), 22);
    assert_eq!(input_report.size_in_bits, 280);

    //Tip Switch
    let ReportField::Variable(field) = &input_report.fields[0] else {
      panic!("Incorrect Field type")
    };
    assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
    assert_eq!(field.bits, 0..1);
    assert_eq!(field.usage, Usage::from(0x000d0042));
    assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
    assert_eq!(field.logical_maximum, LogicalMaximum::from(1));
    assert_eq!(field.physical_minimum, None);
    assert_eq!(field.physical_maximum, None);
    assert_eq!(field.unit, None);
    assert_eq!(field.unit_exponent, None);
    assert_eq!(field.designator_index, None);
    assert_eq!(field.string_index, None);
    let member_of = &field.member_of;
    assert_eq!(member_of.len(), 2);
    assert_eq!(member_of[0].usage, Usage::from(0x000d0004));
    assert_eq!(member_of[1].usage, Usage::from(0x000d0022));
    let ReportField::Padding(field) = &input_report.fields[1] else {
      panic!("Incorrect Field type")
    };
    assert_eq!(field.bits, 1..8);

    //Contact Identifier
    let ReportField::Variable(field) = &input_report.fields[2] else {
      panic!("Incorrect Field type")
    };
    assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
    assert_eq!(field.bits, 8..16);
    assert_eq!(field.usage, Usage::from(0x000d0051));
    assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
    assert_eq!(field.logical_maximum, LogicalMaximum::from(1));
    assert_eq!(field.physical_minimum, None);
    assert_eq!(field.physical_maximum, None);
    assert_eq!(field.unit, None);
    assert_eq!(field.unit_exponent, None);
    assert_eq!(field.designator_index, None);
    assert_eq!(field.string_index, None);
    let member_of = &field.member_of;
    assert_eq!(member_of.len(), 2);
    assert_eq!(member_of[0].usage, Usage::from(0x000d0004));
    assert_eq!(member_of[1].usage, Usage::from(0x000d0022));

    //X/Y axes
    let x_y_reports = [
      (&input_report.fields[3], Usage::from(0x00010030), 16..32, PhysicalMaximum::from(1205)), //X1
      (&input_report.fields[4], Usage::from(0x00010030), 32..48, PhysicalMaximum::from(1205)), //X2
      (&input_report.fields[5], Usage::from(0x00010031), 48..64, PhysicalMaximum::from(906)),  //Y1
      (&input_report.fields[6], Usage::from(0x00010031), 64..80, PhysicalMaximum::from(906)),  //Y2
    ];
    for (field, usage, bits, phy_max) in x_y_reports {
      let ReportField::Variable(field) = field else {
        panic!("Incorrect Field type")
      };
      assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
      assert_eq!(field.bits, bits);
      assert_eq!(field.usage, usage);
      assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
      assert_eq!(field.logical_maximum, LogicalMaximum::from(4095));
      assert_eq!(field.physical_minimum, Some(PhysicalMinimum::from(0)));
      assert_eq!(field.physical_maximum, Some(phy_max));
      assert_eq!(field.unit, Some(Unit::from(19)));
      assert_eq!(field.unit_exponent, Some(UnitExponent::from(14)));
      assert_eq!(field.designator_index, None);
      assert_eq!(field.string_index, None);
      let member_of = &field.member_of;
      assert_eq!(member_of.len(), 2);
      assert_eq!(member_of[0].usage, Usage::from(0x000d0004));
      assert_eq!(member_of[1].usage, Usage::from(0x000d0022));
    }

    //Width, Height
    let width_height_reports = [
      (&input_report.fields[7], Usage::from(0x000d0048), 80..96),
      (&input_report.fields[8], Usage::from(0x000d0049), 96..112),
    ];
    for (field, usage, bits) in width_height_reports {
      let ReportField::Variable(field) = field else {
        panic!("Incorrect Field type")
      };
      assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
      assert_eq!(field.bits, bits);
      assert_eq!(field.usage, usage);
      assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
      assert_eq!(field.logical_maximum, LogicalMaximum::from(4095));
      assert_eq!(field.physical_minimum, Some(PhysicalMinimum::from(0)));
      assert_eq!(field.physical_maximum, Some(PhysicalMaximum::from(906)));
      assert_eq!(field.unit, Some(Unit::from(19)));
      assert_eq!(field.unit_exponent, Some(UnitExponent::from(14)));
      assert_eq!(field.designator_index, None);
      assert_eq!(field.string_index, None);
      let member_of = &field.member_of;
      assert_eq!(member_of.len(), 2);
      assert_eq!(member_of[0].usage, Usage::from(0x000d0004));
      assert_eq!(member_of[1].usage, Usage::from(0x000d0022));
    }

    //Azimuth
    let ReportField::Variable(field) = &input_report.fields[9] else {
      panic!("Incorrect Field type")
    };
    assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
    assert_eq!(field.bits, 112..128);
    assert_eq!(field.usage, Usage::from(0x000d003f));
    assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
    assert_eq!(field.logical_maximum, LogicalMaximum::from(62831));
    assert_eq!(field.physical_minimum, Some(PhysicalMinimum::from(0)));
    assert_eq!(field.physical_maximum, Some(PhysicalMaximum::from(62831)));
    assert_eq!(field.unit, Some(Unit::from(18)));
    assert_eq!(field.unit_exponent, Some(UnitExponent::from(12)));
    assert_eq!(field.designator_index, None);
    assert_eq!(field.string_index, None);
    let member_of = &field.member_of;
    assert_eq!(member_of.len(), 2);
    assert_eq!(member_of[0].usage, Usage::from(0x000d0004));
    assert_eq!(member_of[1].usage, Usage::from(0x000d0022));

    //Tip Switch 2
    let ReportField::Variable(field) = &input_report.fields[10] else {
      panic!("Incorrect Field type")
    };
    assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
    assert_eq!(field.bits, 128..129);
    assert_eq!(field.usage, Usage::from(0x000d0042));
    assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
    assert_eq!(field.logical_maximum, LogicalMaximum::from(1));
    assert_eq!(field.physical_minimum, Some(PhysicalMinimum::from(0)));
    assert_eq!(field.physical_maximum, Some(PhysicalMaximum::from(62831)));
    assert_eq!(field.unit, Some(Unit::from(18)));
    assert_eq!(field.unit_exponent, Some(UnitExponent::from(12)));
    assert_eq!(field.designator_index, None);
    assert_eq!(field.string_index, None);
    let member_of = &field.member_of;
    assert_eq!(member_of.len(), 2);
    assert_eq!(member_of[0].usage, Usage::from(0x000d0004));
    assert_eq!(member_of[1].usage, Usage::from(0x000d0022));
    let ReportField::Padding(field) = &input_report.fields[11] else {
      panic!("Incorrect Field type")
    };
    assert_eq!(field.bits, 129..136);

    //Contact Identifier 2
    let ReportField::Variable(field) = &input_report.fields[12] else {
      panic!("Incorrect Field type")
    };
    assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
    assert_eq!(field.bits, 136..144);
    assert_eq!(field.usage, Usage::from(0x000d0051));
    assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
    assert_eq!(field.logical_maximum, LogicalMaximum::from(1));
    assert_eq!(field.physical_minimum, Some(PhysicalMinimum::from(0)));
    assert_eq!(field.physical_maximum, Some(PhysicalMaximum::from(62831)));
    assert_eq!(field.unit, Some(Unit::from(18)));
    assert_eq!(field.unit_exponent, Some(UnitExponent::from(12)));
    assert_eq!(field.designator_index, None);
    assert_eq!(field.string_index, None);
    let member_of = &field.member_of;
    assert_eq!(member_of.len(), 2);
    assert_eq!(member_of[0].usage, Usage::from(0x000d0004));
    assert_eq!(member_of[1].usage, Usage::from(0x000d0022));

    //X/Y axes 2
    let x_y_reports = [
      (&input_report.fields[13], Usage::from(0x00010030), 144..160, PhysicalMaximum::from(1205)), //X1
      (&input_report.fields[14], Usage::from(0x00010030), 160..176, PhysicalMaximum::from(1205)), //X2
      (&input_report.fields[15], Usage::from(0x00010031), 176..192, PhysicalMaximum::from(906)),  //Y1
      (&input_report.fields[16], Usage::from(0x00010031), 192..208, PhysicalMaximum::from(906)),  //Y2
    ];
    for (field, usage, bits, phy_max) in x_y_reports {
      let ReportField::Variable(field) = field else {
        panic!("Incorrect Field type")
      };
      assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
      assert_eq!(field.bits, bits);
      assert_eq!(field.usage, usage);
      assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
      assert_eq!(field.logical_maximum, LogicalMaximum::from(4095));
      assert_eq!(field.physical_minimum, Some(PhysicalMinimum::from(0)));
      assert_eq!(field.physical_maximum, Some(phy_max));
      assert_eq!(field.unit, Some(Unit::from(19)));
      assert_eq!(field.unit_exponent, Some(UnitExponent::from(14)));
      assert_eq!(field.designator_index, None);
      assert_eq!(field.string_index, None);
      let member_of = &field.member_of;
      assert_eq!(member_of.len(), 2);
      assert_eq!(member_of[0].usage, Usage::from(0x000d0004));
      assert_eq!(member_of[1].usage, Usage::from(0x000d0022));
    }

    //Width, Height 2
    let width_height_reports = [
      (&input_report.fields[17], Usage::from(0x000d0048), 208..224),
      (&input_report.fields[18], Usage::from(0x000d0049), 224..240),
    ];
    for (field, usage, bits) in width_height_reports {
      let ReportField::Variable(field) = field else {
        panic!("Incorrect Field type")
      };
      assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
      assert_eq!(field.bits, bits);
      assert_eq!(field.usage, usage);
      assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
      assert_eq!(field.logical_maximum, LogicalMaximum::from(4095));
      assert_eq!(field.physical_minimum, Some(PhysicalMinimum::from(0)));
      assert_eq!(field.physical_maximum, Some(PhysicalMaximum::from(906)));
      assert_eq!(field.unit, Some(Unit::from(19)));
      assert_eq!(field.unit_exponent, Some(UnitExponent::from(14)));
      assert_eq!(field.designator_index, None);
      assert_eq!(field.string_index, None);
      let member_of = &field.member_of;
      assert_eq!(member_of.len(), 2);
      assert_eq!(member_of[0].usage, Usage::from(0x000d0004));
      assert_eq!(member_of[1].usage, Usage::from(0x000d0022));
    }

    //Azimuth 2
    let ReportField::Variable(field) = &input_report.fields[19] else {
      panic!("Incorrect Field type")
    };
    assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
    assert_eq!(field.bits, 240..256);
    assert_eq!(field.usage, Usage::from(0x000d003f));
    assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
    assert_eq!(field.logical_maximum, LogicalMaximum::from(62831));
    assert_eq!(field.physical_minimum, Some(PhysicalMinimum::from(0)));
    assert_eq!(field.physical_maximum, Some(PhysicalMaximum::from(62831)));
    assert_eq!(field.unit, Some(Unit::from(18)));
    assert_eq!(field.unit_exponent, Some(UnitExponent::from(12)));
    assert_eq!(field.designator_index, None);
    assert_eq!(field.string_index, None);
    let member_of = &field.member_of;
    assert_eq!(member_of.len(), 2);
    assert_eq!(member_of[0].usage, Usage::from(0x000d0004));
    assert_eq!(member_of[1].usage, Usage::from(0x000d0022));

    //Scan Time
    let ReportField::Variable(field) = &input_report.fields[20] else {
      panic!("Incorrect Field type")
    };
    assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
    assert_eq!(field.bits, 256..272);
    assert_eq!(field.usage, Usage::from(0x000d0056));
    assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
    assert_eq!(field.logical_maximum, LogicalMaximum::from(65535));
    assert_eq!(field.physical_minimum, Some(PhysicalMinimum::from(0)));
    assert_eq!(field.physical_maximum, Some(PhysicalMaximum::from(65535)));
    assert_eq!(field.unit, Some(Unit::from(4097)));
    assert_eq!(field.unit_exponent, Some(UnitExponent::from(12)));
    assert_eq!(field.designator_index, None);
    assert_eq!(field.string_index, None);
    let member_of = &field.member_of;
    assert_eq!(member_of.len(), 1);
    assert_eq!(member_of[0].usage, Usage::from(0x000d0004));

    //Contact Count
    let ReportField::Variable(field) = &input_report.fields[21] else {
      panic!("Incorrect Field type")
    };
    assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
    assert_eq!(field.bits, 272..280);
    assert_eq!(field.usage, Usage::from(0x000d0054));
    assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
    assert_eq!(field.logical_maximum, LogicalMaximum::from(127));
    assert_eq!(field.physical_minimum, Some(PhysicalMinimum::from(0)));
    assert_eq!(field.physical_maximum, Some(PhysicalMaximum::from(65535)));
    assert_eq!(field.unit, Some(Unit::from(4097)));
    assert_eq!(field.unit_exponent, Some(UnitExponent::from(12)));
    assert_eq!(field.designator_index, None);
    assert_eq!(field.string_index, None);
    let member_of = &field.member_of;
    assert_eq!(member_of.len(), 1);
    assert_eq!(member_of[0].usage, Usage::from(0x000d0004));

    // End Input Report validation

    // Validate Output Reports (none expected).
    assert_eq!(report_descriptor.output_reports.len(), 0);

    // Validate Feature Reports
    assert_eq!(report_descriptor.features.len(), 2);

    // Contact Count Maximum feature report
    let feature_report = &report_descriptor.features[0];
    assert_eq!(feature_report.fields.len(), 1);

    let ReportField::Variable(field) = &feature_report.fields[0] else {
      panic!("Incorrect Field type")
    };
    assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
    assert_eq!(field.bits, 0..8);
    assert_eq!(field.usage, Usage::from(0x000d0055));
    assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
    assert_eq!(field.logical_maximum, LogicalMaximum::from(2));
    assert_eq!(field.physical_minimum, Some(PhysicalMinimum::from(0)));
    assert_eq!(field.physical_maximum, Some(PhysicalMaximum::from(65535)));
    assert_eq!(field.unit, Some(Unit::from(4097)));
    assert_eq!(field.unit_exponent, Some(UnitExponent::from(12)));
    assert_eq!(field.designator_index, None);
    assert_eq!(field.string_index, None);
    let member_of = &field.member_of;
    assert_eq!(member_of.len(), 1);
    assert_eq!(member_of[0].usage, Usage::from(0x000d0004));

    // Vendor Defined feature report
    let feature_report = &report_descriptor.features[1];
    assert_eq!(feature_report.fields.len(), 256);

    for (index, field) in feature_report.fields.iter().enumerate() {
      let index = index as u32;
      let ReportField::Variable(field) = field else {
        panic!("Incorrect Field type")
      };
      assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
      assert_eq!(field.bits, index * 8..(index + 1) * 8);
      assert_eq!(field.usage, Usage::from(0xff0000C5));
      assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
      assert_eq!(field.logical_maximum, LogicalMaximum::from(255));
      assert_eq!(field.physical_minimum, Some(PhysicalMinimum::from(0)));
      assert_eq!(field.physical_maximum, Some(PhysicalMaximum::from(65535)));
      assert_eq!(field.unit, Some(Unit::from(4097)));
      assert_eq!(field.unit_exponent, Some(UnitExponent::from(12)));
      assert_eq!(field.designator_index, None);
      assert_eq!(field.string_index, None);
      let member_of = &field.member_of;
      assert_eq!(member_of.len(), 1);
      assert_eq!(member_of[0].usage, Usage::from(0x000d0004));
    }
  }

  // Sample descriptor from:
  // https://github.com/microsoft/CFU/blob/89ef6b6ffd221fe82f9ac9d5d0f884a4a965b486/Host/CFUFirmwareSimulation/sys/DmfInterface.c#L33C12-L33C12
  static CFU_REPORT_DESCRIPTOR: &[u8] = &[
    0x06, 0x00, 0xFA, // USAGE_PAGE(0xFA00)
    0x09, 0xF5, // Usage::from(0xF5)
    0xA1, 0x01, // COLLECTION(0x01)
    0x15, 0x00, // LOGICAL_MINIMUM(0)
    0x27, 0xFF, 0xFF, 0xFF, 0xFF, // LOGICAL_MAXIMUM(-1)
    0x85, 0x20, // REPORT_ID(32)
    0x75, 0x08, // REPORT SIZE(8)
    0x95, 0x01, // REPORT COUNT(1)
    0x09, 0x52, // Usage::from(0x52)
    0x81, 0x02, // INPUT(0x02)
    0x85, 0x22, // REPORT_ID(34)
    0x75, 0x20, // REPORT SIZE(32)
    0x95, 0x04, // REPORT COUNT(4)
    0x19, 0x26, // USAGE MIN (0x26)
    0x29, 0x29, // USAGE MAX (0x29)
    0x81, 0x02, // INPUT(0x02)
    0x85, 0x25, // REPORT_ID(37)
    0x75, 0x20, // REPORT SIZE(32)
    0x95, 0x04, // REPORT COUNT(4)
    0x19, 0x1A, // USAGE MIN (0x1A)
    0x29, 0x1D, // USAGE MAX (0x1D)
    0x81, 0x02, // INPUT(0x02)
    0x85, 0x20, // REPORT_ID(32)
    0x75, 0x08, // REPORT SIZE(8)
    0x95, 0x3C, // REPORT COUNT(60)
    0x09, 0x31, // Usage::from(0x31)
    0x92, 0x02, 0x01, // OUTPUT(0102)
    0x85, 0x25, // REPORT_ID(37)
    0x75, 0x20, // REPORT SIZE(32)
    0x95, 0x04, // REPORT COUNT(4)
    0x19, 0x1E, // USAGE MIN (0x1E)
    0x29, 0x21, // USAGE MAX (0x21)
    0x91, 0x02, // OUTPUT(0x02)
    0x85, 0x20, // REPORT_ID(32)
    0x75, 0x08, // REPORT SIZE(8)
    0x95, 0x3C, // REPORT COUNT(60)
    0x09, 0x42, // Usage::from(0x42)
    0xB2, 0x02, 0x01, // FEATURE(0x0102)
    0xC0, // END_COLLECTION()
  ];

  #[test]
  fn test_cfu_report_descriptor() {
    let report_descriptor = ReportDescriptorParser::parse(CFU_REPORT_DESCRIPTOR).unwrap();

    // Validate Input Reports
    assert_eq!(report_descriptor.input_reports.len(), 3);

    // Input Report 1
    let report = &report_descriptor.input_reports[0];
    assert_eq!(report.report_id, Some(ReportId::from(0x20)));
    assert_eq!(report.fields.len(), 1);
    assert_eq!(report.size_in_bits, 8);

    let ReportField::Variable(field) = &report.fields[0] else {
      panic!("Incorrect Field type")
    };
    assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
    assert_eq!(field.bits, 0..8);
    assert_eq!(field.usage, Usage::from(0xFA000052));
    assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
    assert_eq!(field.logical_maximum, LogicalMaximum::from(-1));

    // Input Report 2
    let report = &report_descriptor.input_reports[1];
    assert_eq!(report.report_id, Some(ReportId::from(0x22)));
    assert_eq!(report.fields.len(), 4);
    assert_eq!(report.size_in_bits, 128);

    let fields = [
      (&report.fields[0], 0..32, Usage::from(0xFA000026)),
      (&report.fields[1], 32..64, Usage::from(0xFA000027)),
      (&report.fields[2], 64..96, Usage::from(0xFA000028)),
      (&report.fields[3], 96..128, Usage::from(0xFA000029)),
    ];

    for (field, bits, usage) in fields {
      let ReportField::Variable(field) = field else {
        panic!("Incorrect Field type")
      };
      assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
      assert_eq!(field.bits, bits);
      assert_eq!(field.usage, usage);
      assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
      assert_eq!(field.logical_maximum, LogicalMaximum::from(-1));
    }

    // Input Report 3
    let report = &report_descriptor.input_reports[2];
    assert_eq!(report.report_id, Some(ReportId::from(0x25)));
    assert_eq!(report.fields.len(), 4);
    assert_eq!(report.size_in_bits, 128);

    let fields = [
      (&report.fields[0], 0..32, Usage::from(0xFA00001A)),
      (&report.fields[1], 32..64, Usage::from(0xFA00001B)),
      (&report.fields[2], 64..96, Usage::from(0xFA00001C)),
      (&report.fields[3], 96..128, Usage::from(0xFA00001D)),
    ];

    for (field, bits, usage) in fields {
      let ReportField::Variable(field) = field else {
        panic!("Incorrect Field type")
      };
      assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
      assert_eq!(field.bits, bits);
      assert_eq!(field.usage, usage);
      assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
      assert_eq!(field.logical_maximum, LogicalMaximum::from(-1));
    }

    //Validate Output reports
    assert_eq!(report_descriptor.output_reports.len(), 2);

    let report = &report_descriptor.output_reports[0];
    assert_eq!(report.report_id, Some(ReportId::from(0x20)));
    assert_eq!(report.fields.len(), 60);
    assert_eq!(report.size_in_bits, 480);

    for (index, field) in report.fields.iter().enumerate() {
      let index = index as u32;
      let ReportField::Variable(field) = field else {
        panic!("Incorrect Field type")
      };
      assert_eq!(field.attributes, ReportAttributes { variable: true, buffered_bytes: true, ..Default::default() });
      assert_eq!(field.bits, index * 8..(index + 1) * 8);
      assert_eq!(field.usage, Usage::from(0xFA000031));
    }

    let report = &report_descriptor.output_reports[1];
    assert_eq!(report.report_id, Some(ReportId::from(0x25)));
    assert_eq!(report.fields.len(), 4);
    assert_eq!(report.size_in_bits, 128);

    let fields = [
      (&report.fields[0], 0..32, Usage::from(0xFA00001E)),
      (&report.fields[1], 32..64, Usage::from(0xFA00001F)),
      (&report.fields[2], 64..96, Usage::from(0xFA000020)),
      (&report.fields[3], 96..128, Usage::from(0xFA000021)),
    ];

    for (field, bits, usage) in fields {
      let ReportField::Variable(field) = field else {
        panic!("Incorrect Field type")
      };
      assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
      assert_eq!(field.bits, bits);
      assert_eq!(field.usage, usage);
    }

    //Validate Feature reports
    assert_eq!(report_descriptor.features.len(), 1);

    let report = &report_descriptor.features[0];
    assert_eq!(report.report_id, Some(ReportId::from(0x20)));
    assert_eq!(report.fields.len(), 60);
    assert_eq!(report.size_in_bits, 480);

    for (index, field) in report.fields.iter().enumerate() {
      let index = index as u32;
      let ReportField::Variable(field) = field else {
        panic!("Incorrect Field type")
      };
      assert_eq!(field.attributes, ReportAttributes { variable: true, buffered_bytes: true, ..Default::default() });
      assert_eq!(field.bits, index * 8..(index + 1) * 8);
      assert_eq!(field.usage, Usage::from(0xFA000042));
    }
  }

  static BOOT_KEYBOARD_REPORT_DESCRIPTOR: &[u8] = &[
    0x05, 0x01, // USAGE_PAGE (Generic Desktop)
    0x09, 0x06, // USAGE (Keyboard)
    0xa1, 0x01, // COLLECTION (Application)
    0x75, 0x01, //    REPORT_SIZE (1)
    0x95, 0x08, //    REPORT_COUNT (8)
    0x05, 0x07, //    USAGE_PAGE (Key Codes)
    0x19, 0xE0, //    USAGE_MINIMUM (224)
    0x29, 0xE7, //    USAGE_MAXIMUM (231)
    0x15, 0x00, //    LOGICAL_MINIMUM (0)
    0x25, 0x01, //    LOGICAL_MAXIMUM (1)
    0x81, 0x02, //    INPUT (Data, Var, Abs) (Modifier Byte)
    0x95, 0x01, //    REPORT_COUNT (1)
    0x75, 0x08, //    REPORT_SIZE (8)
    0x81, 0x03, //    INPUT (Const) (Reserved Byte)
    0x95, 0x05, //    REPORT_COUNT (5)
    0x75, 0x01, //    REPORT_SIZE (1)
    0x05, 0x08, //    USAGE_PAGE (LEDs)
    0x19, 0x01, //    USAGE_MINIMUM (1)
    0x29, 0x05, //    USAGE_MAXIMUM (5)
    0x91, 0x02, //    OUTPUT (Data, Var, Abs) (LED report)
    0x95, 0x01, //    REPORT_COUNT (1)
    0x75, 0x03, //    REPORT_SIZE (3)
    0x91, 0x02, //    OUTPUT (Constant) (LED report padding)
    0x95, 0x06, //    REPORT_COUNT (6)
    0x75, 0x08, //    REPORT_SIZE (8)
    0x15, 0x00, //    LOGICAL_MINIMUM (0)
    0x26, 0xff, 00, //    LOGICAL_MAXIMUM (255)
    0x05, 0x07, //    USAGE_PAGE (Key Codes)
    0x19, 0x00, //    USAGE_MINIMUM (0)
    0x2a, 0xff, 00, //    USAGE_MAXIMUM (255)
    0x81, 0x00, //    INPUT (Data, Array)
    0xc0, // END_COLLECTION
  ];

  #[test]
  fn test_keyboard_report_descriptor() {
    let report_descriptor = ReportDescriptorParser::parse(BOOT_KEYBOARD_REPORT_DESCRIPTOR).unwrap();

    // Verify input report
    assert_eq!(report_descriptor.input_reports.len(), 1);

    let report = &report_descriptor.input_reports[0];
    assert_eq!(report.report_id, None);
    assert_eq!(report.fields.len(), 15);
    assert_eq!(report.size_in_bits, 64);

    // Modifier keys
    let mut bit: u32 = 0;
    for (field, usage) in report.fields[0..8].iter().zip(0xe0..0xe7) {
      let ReportField::Variable(field) = field else {
        panic!("Incorrect Field type")
      };
      assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
      assert_eq!(field.bits, bit..bit + 1);
      assert_eq!(field.usage, Usage::from_page_and_id(Some(UsagePage::from(0x07)), Usage::from(usage)));
      assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
      assert_eq!(field.logical_maximum, LogicalMaximum::from(1));
      bit += 1;
    }

    // Reserved byte
    let ReportField::Padding(field) = &report.fields[8] else {
      panic!("Incorrect Field type")
    };
    assert_eq!(field.bits, 8..16);

    // Keycode array
    bit = 16;
    for field in &report.fields[9..15] {
      let ReportField::Array(field) = field else {
        panic!("Incorrect Field type")
      };
      assert_eq!(field.attributes, ReportAttributes { ..Default::default() });
      assert_eq!(field.bits, bit..bit + 8);
      assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
      assert_eq!(field.logical_maximum, LogicalMaximum::from(255));
      assert_eq!(field.usage_list, Vec::from([UsageRange::from(0x00070000..=0x000700ff)]));
      bit += 8;
    }

    // Verify output report
    assert_eq!(report_descriptor.output_reports.len(), 1);

    let report = &report_descriptor.output_reports[0];
    assert_eq!(report.report_id, None);
    assert_eq!(report.fields.len(), 6);
    assert_eq!(report.size_in_bits, 8);

    // LEDs
    bit = 0;
    for (field, usage) in report.fields[0..5].iter().zip(0x01..0x05) {
      let ReportField::Variable(field) = field else {
        panic!("Incorrect Field type")
      };
      assert_eq!(field.attributes, ReportAttributes { variable: true, ..Default::default() });
      assert_eq!(field.bits, bit..bit + 1);
      assert_eq!(field.usage, Usage::from_page_and_id(Some(UsagePage::from(0x08)), Usage::from(usage)));
      assert_eq!(field.logical_minimum, LogicalMinimum::from(0));
      assert_eq!(field.logical_maximum, LogicalMaximum::from(1));
      bit += 1;
    }
  }

  static MINIMAL_BOOT_KEYBOARD_REPORT_DESCRIPTOR: &[u8] = &[
    0x05, 0x01, // USAGE_PAGE (Generic Desktop)
    0x09, 0x06, // USAGE (Keyboard)
    0xa1, 0x01, // COLLECTION (Application)
    0x75, 0x01, //    REPORT_SIZE (1)
    0x95, 0x08, //    REPORT_COUNT (8)
    0x05, 0x07, //    USAGE_PAGE (Key Codes)
    0x19, 0xE0, //    USAGE_MINIMUM (224)
    0x29, 0xE7, //    USAGE_MAXIMUM (231)
    0x15, 0x00, //    LOGICAL_MINIMUM (0)
    0x25, 0x01, //    LOGICAL_MAXIMUM (1)
    0x81, 0x02, //    INPUT (Data, Var, Abs) (Modifier Byte)
    0xc0, // END_COLLECTION
  ];

  static BOGUS_BOOT_KEYBOARD_REPORT_DESCRIPTOR_INVALID_MAIN_ITEM: &[u8] = &[
    0xf0, 0x01, // USAGE_PAGE (Generic Desktop) (invalid main item)
    0x09, 0x06, // USAGE (Keyboard)
    0xa1, 0x01, // COLLECTION (Application)
    0x75, 0x01, //    REPORT_SIZE (1)
    0x95, 0x08, //    REPORT_COUNT (8)
    0x05, 0x07, //    USAGE_PAGE (Key Codes)
    0x19, 0xE0, //    USAGE_MINIMUM (224)
    0x29, 0xE7, //    USAGE_MAXIMUM (231)
    0x15, 0x00, //    LOGICAL_MINIMUM (0)
    0x25, 0x01, //    LOGICAL_MAXIMUM (1)
    0x81, 0x02, //    INPUT (Data, Var, Abs) (Modifier Byte)
    0xc0, // END_COLLECTION
  ];

  static BOGUS_BOOT_KEYBOARD_REPORT_DESCRIPTOR_INVALID_GLOBAL_ITEM: &[u8] = &[
    0x05, 0x01, // USAGE_PAGE (Generic Desktop)
    0x09, 0x06, // USAGE (Keyboard)
    0xa1, 0x01, // COLLECTION (Application)
    0xf5, 0x01, //    REPORT_SIZE (1) (invalid global item)
    0x95, 0x08, //    REPORT_COUNT (8)
    0x05, 0x07, //    USAGE_PAGE (Key Codes)
    0x19, 0xE0, //    USAGE_MINIMUM (224)
    0x29, 0xE7, //    USAGE_MAXIMUM (231)
    0x15, 0x00, //    LOGICAL_MINIMUM (0)
    0x25, 0x01, //    LOGICAL_MAXIMUM (1)
    0x81, 0x02, //    INPUT (Data, Var, Abs) (Modifier Byte)
    0xc0, // END_COLLECTION
  ];

  static BOGUS_BOOT_KEYBOARD_REPORT_DESCRIPTOR_INVALID_LOCAL_ITEM: &[u8] = &[
    0x05, 0x01, // USAGE_PAGE (Generic Desktop)
    0x09, 0x06, // USAGE (Keyboard)
    0xa1, 0x01, // COLLECTION (Application)
    0x75, 0x01, //    REPORT_SIZE (1)
    0x95, 0x08, //    REPORT_COUNT (8)
    0x05, 0x07, //    USAGE_PAGE (Key Codes)
    0xf9, 0xE0, //    USAGE_MINIMUM (224) (invalid local item)
    0x29, 0xE7, //    USAGE_MAXIMUM (231)
    0x15, 0x00, //    LOGICAL_MINIMUM (0)
    0x25, 0x01, //    LOGICAL_MAXIMUM (1)
    0x81, 0x02, //    INPUT (Data, Var, Abs) (Modifier Byte)
    0xc0, // END_COLLECTION
  ];

  static BOGUS_BOOT_KEYBOARD_REPORT_DESCRIPTOR_INVALID_RESERVED_ITEM: &[u8] = &[
    0x05, 0x01, // USAGE_PAGE (Generic Desktop)
    0x09, 0x06, // USAGE (Keyboard)
    0xa1, 0x01, // COLLECTION (Application)
    0x75, 0x01, //    REPORT_SIZE (1)
    0x95, 0x08, //    REPORT_COUNT (8)
    0x05, 0x07, //    USAGE_PAGE (Key Codes)
    0x19, 0xE0, //    USAGE_MINIMUM (224)
    0x29, 0xE7, //    USAGE_MAXIMUM (231)
    0x15, 0x00, //    LOGICAL_MINIMUM (0)
    0xff, 0x01, //    LOGICAL_MAXIMUM (1) (invalid reserved item)
    0x81, 0x02, //    INPUT (Data, Var, Abs) (Modifier Byte)
    0xc0, // END_COLLECTION
  ];

  static BOGUS_BOOT_KEYBOARD_REPORT_DESCRIPTOR_INVALID_POP: &[u8] = &[
    0x05, 0x01, // USAGE_PAGE (Generic Desktop)
    0x09, 0x06, // USAGE (Keyboard)
    0xa1, 0x01, // COLLECTION (Application)
    0x75, 0x01, //    REPORT_SIZE (1)
    0x95, 0x08, //    REPORT_COUNT (8)
    0x05, 0x07, //    USAGE_PAGE (Key Codes)
    0x19, 0xE0, //    USAGE_MINIMUM (224)
    0xb4, //     POP  (Invalid)
    0x29, 0xE7, //    USAGE_MAXIMUM (231)
    0x15, 0x00, //    LOGICAL_MINIMUM (0)
    0x25, 0x01, //    LOGICAL_MAXIMUM (1)
    0x81, 0x02, //    INPUT (Data, Var, Abs) (Modifier Byte)
    0xc0, // END_COLLECTION
  ];

  static BOGUS_BOOT_KEYBOARD_REPORT_DESCRIPTOR_DELIMITER: &[u8] = &[
    0x05, 0x01, // USAGE_PAGE (Generic Desktop)
    0x09, 0x06, // USAGE (Keyboard)
    0xa1, 0x01, // COLLECTION (Application)
    0x75, 0x01, //    REPORT_SIZE (1)
    0x95, 0x08, //    REPORT_COUNT (8)
    0x05, 0x07, //    USAGE_PAGE (Key Codes)
    0xA8, //    DELIMITER (unsupported)
    0x19, 0xE0, //    USAGE_MINIMUM (224)
    0x29, 0xE7, //    USAGE_MAXIMUM (231)
    0x15, 0x00, //    LOGICAL_MINIMUM (0)
    0x25, 0x01, //    LOGICAL_MAXIMUM (1)
    0x81, 0x02, //    INPUT (Data, Var, Abs) (Modifier Byte)
    0xc0, // END_COLLECTION
  ];

  static BOGUS_BOOT_KEYBOARD_REPORT_DESCRIPTOR_NO_REPORT_SIZE: &[u8] = &[
    0x05, 0x01, // USAGE_PAGE (Generic Desktop)
    0x09, 0x06, // USAGE (Keyboard)
    0xa1, 0x01, // COLLECTION (Application)
    0x95, 0x08, //    REPORT_COUNT (8)
    0x05, 0x07, //    USAGE_PAGE (Key Codes)
    0x19, 0xE0, //    USAGE_MINIMUM (224)
    0x29, 0xE7, //    USAGE_MAXIMUM (231)
    0x15, 0x00, //    LOGICAL_MINIMUM (0)
    0x25, 0x01, //    LOGICAL_MAXIMUM (1)
    0x81, 0x02, //    INPUT (Data, Var, Abs) (Modifier Byte)
    0xc0, // END_COLLECTION
  ];

  static BOGUS_BOOT_KEYBOARD_REPORT_DESCRIPTOR_NO_REPORT_COUNT: &[u8] = &[
    0x05, 0x01, // USAGE_PAGE (Generic Desktop)
    0x09, 0x06, // USAGE (Keyboard)
    0xa1, 0x01, // COLLECTION (Application)
    0x75, 0x01, //    REPORT_SIZE (1)
    0x05, 0x07, //    USAGE_PAGE (Key Codes)
    0x19, 0xE0, //    USAGE_MINIMUM (224)
    0x29, 0xE7, //    USAGE_MAXIMUM (231)
    0x15, 0x00, //    LOGICAL_MINIMUM (0)
    0x25, 0x01, //    LOGICAL_MAXIMUM (1)
    0x81, 0x02, //    INPUT (Data, Var, Abs) (Modifier Byte)
    0xc0, // END_COLLECTION
  ];

  static BOGUS_MINIMAL_BOOT_KEYBOARD_REPORT_DESCRIPTOR_NO_LOG_MIN: &[u8] = &[
    0x05, 0x01, // USAGE_PAGE (Generic Desktop)
    0x09, 0x06, // USAGE (Keyboard)
    0xa1, 0x01, // COLLECTION (Application)
    0x75, 0x01, //    REPORT_SIZE (1)
    0x95, 0x08, //    REPORT_COUNT (8)
    0x05, 0x07, //    USAGE_PAGE (Key Codes)
    0x19, 0xE0, //    USAGE_MINIMUM (224)
    0x29, 0xE7, //    USAGE_MAXIMUM (231)
    0x25, 0x01, //    LOGICAL_MAXIMUM (1)
    0x81, 0x02, //    INPUT (Data, Var, Abs) (Modifier Byte)
    0xc0, // END_COLLECTION
  ];

  static BOGUS_MINIMAL_BOOT_KEYBOARD_REPORT_DESCRIPTOR_NO_LOG_MAX: &[u8] = &[
    0x05, 0x01, // USAGE_PAGE (Generic Desktop)
    0x09, 0x06, // USAGE (Keyboard)
    0xa1, 0x01, // COLLECTION (Application)
    0x75, 0x01, //    REPORT_SIZE (1)
    0x95, 0x08, //    REPORT_COUNT (8)
    0x05, 0x07, //    USAGE_PAGE (Key Codes)
    0x19, 0xE0, //    USAGE_MINIMUM (224)
    0x29, 0xE7, //    USAGE_MAXIMUM (231)
    0x15, 0x00, //    LOGICAL_MINIMUM (0)
    0x81, 0x02, //    INPUT (Data, Var, Abs) (Modifier Byte)
    0xc0, // END_COLLECTION
  ];

  static MINIMAL_BOOT_KEYBOARD_REPORT_DESCRIPTOR_INVALID_LOG_RANGE: &[u8] = &[
    0x05, 0x01, // USAGE_PAGE (Generic Desktop)
    0x09, 0x06, // USAGE (Keyboard)
    0xa1, 0x01, // COLLECTION (Application)
    0x75, 0x01, //    REPORT_SIZE (1)
    0x95, 0x08, //    REPORT_COUNT (8)
    0x05, 0x07, //    USAGE_PAGE (Key Codes)
    0x19, 0xE0, //    USAGE_MINIMUM (224)
    0x29, 0xE7, //    USAGE_MAXIMUM (231)
    0x15, 0x01, //    LOGICAL_MINIMUM (1)
    0x25, 0x00, //    LOGICAL_MAXIMUM (0)
    0x81, 0x02, //    INPUT (Data, Var, Abs) (Modifier Byte)
    0xc0, // END_COLLECTION
  ];

  static MINIMAL_BOOT_KEYBOARD_REPORT_DESCRIPTOR_INVALID_LOG_RANGE2: &[u8] = &[
    0x05, 0x01, // USAGE_PAGE (Generic Desktop)
    0x09, 0x06, // USAGE (Keyboard)
    0xa1, 0x01, // COLLECTION (Application)
    0x75, 0x01, //    REPORT_SIZE (1)
    0x95, 0x08, //    REPORT_COUNT (8)
    0x05, 0x07, //    USAGE_PAGE (Key Codes)
    0x19, 0xE0, //    USAGE_MINIMUM (224)
    0x29, 0xE7, //    USAGE_MAXIMUM (231)
    0x15, 0xff, //    LOGICAL_MINIMUM (-1)
    0x25, 0xfe, //    LOGICAL_MAXIMUM (-2)
    0x81, 0x02, //    INPUT (Data, Var, Abs) (Modifier Byte)
    0xc0, // END_COLLECTION
  ];

  #[test]
  fn test_bogus_descriptors_should_not_parse() {
    let _ = ReportDescriptorParser::parse(MINIMAL_BOOT_KEYBOARD_REPORT_DESCRIPTOR).unwrap();

    assert_eq!(
      ReportDescriptorParser::parse(BOGUS_BOOT_KEYBOARD_REPORT_DESCRIPTOR_INVALID_MAIN_ITEM).err(),
      Some(ReportDescriptorError::InvalidMainItem)
    );

    assert_eq!(
      ReportDescriptorParser::parse(BOGUS_BOOT_KEYBOARD_REPORT_DESCRIPTOR_INVALID_GLOBAL_ITEM).err(),
      Some(ReportDescriptorError::InvalidGlobalItem)
    );

    assert_eq!(
      ReportDescriptorParser::parse(BOGUS_BOOT_KEYBOARD_REPORT_DESCRIPTOR_INVALID_LOCAL_ITEM).err(),
      Some(ReportDescriptorError::InvalidLocalItem)
    );

    assert_eq!(
      ReportDescriptorParser::parse(BOGUS_BOOT_KEYBOARD_REPORT_DESCRIPTOR_INVALID_RESERVED_ITEM).err(),
      Some(ReportDescriptorError::ReservedItemNotSupported)
    );

    assert_eq!(
      ReportDescriptorParser::parse(BOGUS_BOOT_KEYBOARD_REPORT_DESCRIPTOR_INVALID_POP).err(),
      Some(ReportDescriptorError::InvalidPop,)
    );

    assert_eq!(
      ReportDescriptorParser::parse(BOGUS_BOOT_KEYBOARD_REPORT_DESCRIPTOR_DELIMITER).err(),
      Some(ReportDescriptorError::DelimiterNotSupported,)
    );

    assert_eq!(
      ReportDescriptorParser::parse(BOGUS_BOOT_KEYBOARD_REPORT_DESCRIPTOR_NO_REPORT_SIZE).err(),
      Some(ReportDescriptorError::InvalidReportNoSize)
    );

    assert_eq!(
      ReportDescriptorParser::parse(BOGUS_BOOT_KEYBOARD_REPORT_DESCRIPTOR_NO_REPORT_COUNT).err(),
      Some(ReportDescriptorError::InvalidReportNoCount)
    );

    assert_eq!(
      ReportDescriptorParser::parse(BOGUS_MINIMAL_BOOT_KEYBOARD_REPORT_DESCRIPTOR_NO_LOG_MIN).err(),
      Some(ReportDescriptorError::InvalidReportNoLogicalMin)
    );

    assert_eq!(
      ReportDescriptorParser::parse(BOGUS_MINIMAL_BOOT_KEYBOARD_REPORT_DESCRIPTOR_NO_LOG_MAX).err(),
      Some(ReportDescriptorError::InvalidReportNoLogicalMax)
    );

    assert_eq!(
      ReportDescriptorParser::parse(MINIMAL_BOOT_KEYBOARD_REPORT_DESCRIPTOR_INVALID_LOG_RANGE).err(),
      Some(ReportDescriptorError::InvalidReportLogicalRange)
    );

    assert_eq!(
      ReportDescriptorParser::parse(MINIMAL_BOOT_KEYBOARD_REPORT_DESCRIPTOR_INVALID_LOG_RANGE2).err(),
      Some(ReportDescriptorError::InvalidReportLogicalRange)
    );
  }
}
