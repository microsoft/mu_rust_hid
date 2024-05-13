//! HID Report Descriptor Parser Utility
//!
//! Simple command line utility that supports parsing descriptors and printing the results.
//!
//! Demonstrates the usage of the [`hidparser`] crate.
//!
//! # Usage
//!
//! `cargo run --example hidparse -- --path .\examples\samples\boot_keyboard.bin`
//!
//! ## License
//!
//! Copyright (c) Microsoft Corporation. All rights reserved.
//!
//! SPDX-License-Identifier: BSD-2-Clause-Patent
//!

use std::{
  error::Error,
  fs::{self, File},
  io::BufReader,
  iter,
  path::PathBuf,
};

use clap::{Parser, ValueEnum};
use hidparser::{parse_report_descriptor, ReportDescriptor, ReportField};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

#[derive(Parser, Debug, Clone, PartialEq, Eq, ValueEnum)]
enum ReportType {
  Input,
  Output,
  Feature,
}

/// Simple command line utility that supports parsing descriptors and printing the results.
///
/// Copyright (c) Microsoft Corporation. All rights reserved.
///
/// SPDX-License-Identifier: BSD-2-Clause-Patent
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
#[serde(rename_all = "PascalCase")]
struct UsageId {
  id: u16,
  name: String,
  kinds: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "PascalCase")]
struct UsagePage {
  kind: String,
  id: u16,
  name: String,
  usage_ids: Vec<UsageId>,
  #[serde(skip)]
  _usage_id_generator: Map<String, Value>,
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

fn usage_tables() -> Result<HidUsageTable, Box<dyn Error>> {
  let file = File::open(
    [env!("CARGO_MANIFEST_DIR"), "examples", "resources", "HidUsageTables.json"].iter().collect::<PathBuf>(),
  )?;
  let us = serde_json::from_reader(BufReader::new(file))?;
  Ok(us)
}

fn main() -> Result<(), Box<dyn Error>> {
  let args = Arguments::parse();
  let raw_descriptor = fs::read(args.path)?;

  let ReportDescriptor { input_reports, output_reports, features } =
    parse_report_descriptor(&raw_descriptor).map_err(|_| "Failed to parse descriptor.")?;

  let reports = (iter::repeat(ReportType::Input).zip(input_reports))
    .chain(iter::repeat(ReportType::Output).zip(output_reports))
    .chain(iter::repeat(ReportType::Feature).zip(features));

  let filtered_reports = reports.filter(|(report_type, report)| match (&args.report_type, args.report_id) {
    (Some(requested_type), _) if requested_type != report_type => false,
    (_, id @ Some(_)) if id != report.report_id.map(|report_id| report_id.into()) => false,
    _ => true,
  });

  let ut = usage_tables()?;

  for (report_type, report) in filtered_reports {
    println!("{report_type:?} id: {:?}", report.report_id);
    for field in &report.fields {
      match field {
        ReportField::Variable(v) => {
          match v.bits.len() {
            1 => print!("\tbit: {}", v.bits.start),
            _ => print!("\tbits: {}..={}", v.bits.start, v.bits.end - 1),
          };
          print!("\tusage: ({:?}:{:?})\t- ", v.usage.page(), v.usage.id());
          let usage_page = ut.UsagePages.iter().find(|x| x.id == v.usage.page());
          if let Some(page) = usage_page {
            print!(" page: {:?} ", page.name);
            let usage = page.usage_ids.iter().find(|x| x.id == v.usage.id());
            if let Some(usage) = usage {
              print!("id: {:?}", usage.name);
            } else {
              print!("id: <unrecognized>");
            }
          } else {
            print!("page: <unrecognized>")
          }
          println!()
        }
        ReportField::Array(a) => {
          let bits = match a.bits.len() {
            1 => format!("bit: {}", a.bits.start),
            _ => format!("bits: {}..={}", a.bits.start, a.bits.end - 1),
          };
          println!("\t{bits}\tusages: {:?} \t-", a.usage_list)
        }
        ReportField::Padding(p) => {
          let bits = match p.bits.len() {
            1 => format!("bit: {}", p.bits.start),
            _ => format!("bits: {}..={}", p.bits.start, p.bits.end - 1),
          };
          println!("\t{bits}\tpadding");
        }
      }
    }
  }

  Ok(())
}
