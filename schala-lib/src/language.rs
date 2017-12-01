extern crate colored;

use self::colored::*;

pub struct LLVMCodeString(pub String);

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct EvalOptions {
  pub debug_tokens: bool,
  pub debug_parse: bool,
  pub debug_type: bool,
  pub debug_symbol_table: bool,
  pub show_llvm_ir: bool,
  pub trace_evaluation: bool,
  pub compile: bool,
}

#[derive(Debug, Default)]
pub struct ReplOutput {
  output: String,
  artifacts: Vec<TraceArtifact>
}

impl ReplOutput {
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

/*
//TODO I'll probably wanna implement this later
#[derive(Debug)]
pub struct CompilationOutput {
  output: LLVMCodeString,
  artifacts: Vec<TraceArtifact>,
}
*/

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
  fn evaluate_in_repl(&mut self, input: &str, eval_options: &EvalOptions) -> ReplOutput;
  fn get_language_name(&self) -> String;
  fn get_source_file_suffix(&self) -> String;
  fn compile(&mut self, _input: &str) -> LLVMCodeString {
    LLVMCodeString("".to_string())
  }
  fn can_compile(&self) -> bool {
    false
  }
}