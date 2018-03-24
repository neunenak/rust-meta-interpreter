use std::collections::HashMap;
use colored::*;
use std::fmt::Write;

pub struct LLVMCodeString(pub String);

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct EvalOptions {
  pub debug: DebugOptions,
  pub execution_method: ExecutionMethod
}
#[derive(Debug, Serialize, Deserialize)]
pub enum ExecutionMethod {
  Compile,
  Interpret,
}
impl Default for ExecutionMethod {
  fn default() -> ExecutionMethod {
    ExecutionMethod::Interpret
  }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct DebugOptions {
  pub tokens: bool,
  pub parse_tree: bool,
  pub ast: bool,
  pub type_checking: bool,
  pub symbol_table: bool,
  pub evaluation: bool,
  pub llvm_ir: bool,
}

#[derive(Debug, Default)]
pub struct LanguageOutput {
  output: String,
  artifacts: Vec<TraceArtifact>,
  pub failed: bool,
}

impl LanguageOutput {
  pub fn add_artifact(&mut self, artifact: TraceArtifact) {
    self.artifacts.push(artifact);
  }
  pub fn add_output(&mut self, output: String) {
    self.output = output;
  }

  pub fn to_string(&self) -> String {
    let mut acc = String::new();
    for line in self.artifacts.iter() {
      acc.push_str(&line.debug_output.color(line.text_color).to_string());
      acc.push_str(&"\n");
    }
    acc.push_str(&self.output);
    acc
  }

  pub fn print_to_screen(&self) {
    for line in self.artifacts.iter() {
      let color = line.text_color;
      let stage = line.stage_name.color(color).to_string();
      let output = line.debug_output.color(color).to_string();
      println!("{}: {}", stage, output);
    }
    println!("{}", self.output);
  }
}

#[derive(Debug, Default)]
pub struct UnfinishedComputation {
  artifacts: HashMap<String, TraceArtifact>,
}

#[derive(Debug)]
pub struct FinishedComputation {
  artifacts: HashMap<String, TraceArtifact>,
  text_output: Result<String, String>,
}

impl UnfinishedComputation {
  pub fn add_artifact(&mut self, artifact: TraceArtifact) {
    self.artifacts.insert(artifact.stage_name.clone(), artifact);
  }
  pub fn output(self, output: Result<String, String>) -> FinishedComputation {
    FinishedComputation {
      artifacts: self.artifacts,
      text_output: output
    }
  }
}

impl FinishedComputation {
  pub fn to_repl(&self) -> String {
    match self.text_output {
      Ok(ref output) => {
        let mut buf = String::new();
        for stage in ["tokens", "parse_trace", "ast", "symbol_table", "type_check"].iter() {
          if let Some(artifact) = self.artifacts.get(&stage.to_string()) {
            let color = artifact.text_color;
            let stage = stage.color(color).bold();
            let output = artifact.debug_output.color(color);
            write!(&mut buf, "{}: {}\n", stage, output);
          }
        }
        write!(&mut buf, "{}", output);
        buf
      }
      Err(ref s) => format!("{} {}", "Error: ".red().bold(), s)
    }
  }
  pub fn to_noninteractive(&self) -> Option<String> {
    match self.text_output {
      Ok(ref s) => None,
      Err(ref s) => Some(format!("{} {}", "Error: ".red().bold(), s))
    }
  }
}

#[derive(Debug)]
pub struct TraceArtifact {
  stage_name: String,
  debug_output: String,
  text_color: &'static str,
}

impl TraceArtifact {
  pub fn new(stage: &str, debug: String) -> TraceArtifact {
    let color = match stage {
      "parse_trace"  | "ast" => "red",
      "tokens" => "green",
      "type_check" => "magenta",
      _ => "blue",
    };
    TraceArtifact { stage_name: stage.to_string(), debug_output: debug, text_color: color}
  }

  pub fn new_parse_trace(trace: Vec<String>) -> TraceArtifact {
    let mut output = String::new();

    for t in trace {
      output.push_str(&t);
      output.push_str("\n");
    }

    TraceArtifact { stage_name: "parse_trace".to_string(), debug_output: output, text_color: "red"}
  }
}

pub trait ProgrammingLanguageInterface {
  /* old */
  fn evaluate_in_repl(&mut self, input: &str, eval_options: &EvalOptions) -> LanguageOutput {
    LanguageOutput { output: format!("Defunct"), artifacts: vec![], failed: false }
  }
  fn evaluate_noninteractive(&mut self, input: &str, eval_options: &EvalOptions) -> LanguageOutput {
    self.evaluate_in_repl(input, eval_options)
  }
  /* old */


  fn execute(&mut self, input: &str, eval_options: &EvalOptions) -> FinishedComputation {
    FinishedComputation { artifacts: HashMap::new(), text_output: Err(format!("REPL evaluation not implemented")) }
  }
  fn get_language_name(&self) -> String;
  fn get_source_file_suffix(&self) -> String;
}
