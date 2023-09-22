//! HID Report Descriptor Parser Utility
//!
//! Simple command line utility that supports parsing descriptors and printing the results.
//!
//! Demonstrates the usage of the [`hid_report_descriptor_parser`] crate.
//!
//! # Usage
//!
//! `hidparse.exe --path .\samples\boot_keyboard.bin`
//!
//! or
//!
//! `cargo run -- --path .\samples\boot_keyboard.bin`
//!
//! ## License
//!
//! Copyright (C) Microsoft Corporation. All rights reserved.
//!
//! SPDX-License-Identifier: BSD-2-Clause-Patent
//!
use std::{
  fs::{self, File},
  io::BufReader,
};

use clap::{Parser, ValueEnum};
use hid_report_descriptor_parser::{parse_report_descriptor, ReportField};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

#[derive(Parser, Debug, Clone, PartialEq, Eq, ValueEnum)]
enum ReportType {
  Input,
  Output,
  Feature,
}

/// Arguments
#[derive(Parser, Debug)]
struct Arguments {
  /// The path containing descriptor binary file.
  #[arg(short, long)]
  path: std::path::PathBuf,

  /// Type of report to list
  #[arg(short, long)]
  report_type: Option<ReportType>,

  /// Id of report to list
  #[arg(short = 'i', long)]
  report_id: Option<u32>,
}

#[derive(Serialize, Deserialize, Debug)]
#[allow(non_snake_case)]
struct UsageId {
  Id: u16,
  Name: String,
  Kinds: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug)]
#[allow(non_snake_case)]
struct UsagePage {
  Kind: String,
  Id: u16,
  Name: String,
  UsageIds: Vec<UsageId>,
  #[serde(skip)]
  _UsageIdGenerator: Map<String, Value>,
}

#[derive(Serialize, Deserialize, Debug)]
#[allow(non_snake_case)]
struct HidUsageTable {
  UsageTableVersion: u8,
  UsageTableRevision: u8,
  UsageTableSubRevisionInternal: u8,
  LastGenerated: String,
  UsagePages: Vec<UsagePage>,
}

fn usage_tables() -> HidUsageTable {
  let file = File::open(concat!(env!("CARGO_MANIFEST_DIR"), "/resources/", "HidUsageTables.json"))
    .expect("Failed to open HidUsageTable JSON");
  serde_json::from_reader(BufReader::new(file)).expect("Error parsing HidUsageTable JSON")
}

fn main() {
  let args = Arguments::parse();

  let raw_descriptor = fs::read(args.path).expect("Failed to read raw descriptor");

  let parsed_descriptor = parse_report_descriptor(&raw_descriptor).expect("Failed to parse descriptor.");

  let mut reports: Vec<_> = std::iter::repeat(ReportType::Input).zip(parsed_descriptor.input_reports).collect();
  reports.extend(std::iter::repeat(ReportType::Output).zip(parsed_descriptor.output_reports));
  reports.extend(std::iter::repeat(ReportType::Feature).zip(parsed_descriptor.features));

  let filtered_reports = reports.iter().filter(|(report_type, report)| {
    if let Some(requested_type) = &args.report_type {
      if requested_type != report_type {
        return false;
      }
    }
    if let Some(requested_id) = args.report_id {
      match report.report_id {
        Some(id) => {
          let id: u32 = id.into();
          if id != requested_id {
            return false;
          }
        }
        None => return false,
      }
    }
    true
  });

  let ut = usage_tables();

  for (report_type, report) in filtered_reports {
    println!("{report_type:?} id: {:?}", report.report_id);
    for field in &report.fields {
      match field {
        ReportField::Variable(v) => {
          if v.bits.len() == 1 {
            print!("\tbit:  {:?}", v.bits.start);
          } else {
            print!("\tbits: {:?}..{:?}", v.bits.start, v.bits.end - 1);
          }
          print!("\tusage: ({:?}:{:?})\t- ", v.usage.page(), v.usage.id());
          let usage_page = ut.UsagePages.iter().find(|x| x.Id == v.usage.page());
          if let Some(page) = usage_page {
            print!(" page: {:?} ", page.Name);
            let usage = page.UsageIds.iter().find(|x| x.Id == v.usage.id());
            if let Some(usage) = usage {
              print!("id: {:?}", usage.Name);
            } else {
              print!("id: <unrecognized>");
            }
          } else {
            print!("page: <unrecognized>")
          }
          println!("")
        }
        ReportField::Array(a) => {
          if a.bits.len() == 1 {
            print!("\tbit:  {:?}", a.bits.start);
          } else {
            print!("\tbits: {:?}..{:?}", a.bits.start, a.bits.end - 1);
          }
          print!("\tusages: {:?})\t- ", a.usage_list);
          println!("")
        }
        ReportField::Padding(p) => {
          if p.bits.len() == 1 {
            print!("\tbit:  {:?}", p.bits.start);
          } else {
            print!("\tbits: {:?}..{:?}", p.bits.start, p.bits.end - 1);
          }
          println!("\tpadding");
        }
      }
    }
  }
}
