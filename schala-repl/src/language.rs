use std::collections::{HashSet, HashMap};
use colored::*;
use std::fmt::Write;

pub struct LLVMCodeString(pub String);

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct EvalOptions {
  pub execution_method: ExecutionMethod,
  pub debug_passes: HashSet<String>,
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

#[derive(Debug, Default)]
pub struct UnfinishedComputation {
  artifacts: Vec<(String, TraceArtifact)>,
}

#[derive(Debug)]
pub struct FinishedComputation {
  artifacts: Vec<(String, TraceArtifact)>,
  text_output: Result<String, String>,
}

impl UnfinishedComputation {
  pub fn add_artifact(&mut self, artifact: TraceArtifact) {
    self.artifacts.push((artifact.stage_name.clone(), artifact));
  }
  pub fn finish(self, text_output: Result<String, String>) -> FinishedComputation {
    FinishedComputation {
      artifacts: self.artifacts,
      text_output
    }
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
    let mut buf = String::new();
    for (stage, artifact) in self.artifacts.iter() {
      let color = artifact.text_color;
      let stage = stage.color(color).bold();
      let output = artifact.debug_output.color(color);
      write!(&mut buf, "{}: {}\n", stage, output).unwrap();
    }

    match self.text_output {
      Ok(ref output) => write!(&mut buf, "{}", output).unwrap(),
      Err(ref err) => write!(&mut buf, "{} {}", "Error: ".red().bold(), err).unwrap(),
    }
    buf
  }
  pub fn to_noninteractive(&self) -> Option<String> {
    match self.text_output {
      Ok(_) => {
        let mut buf = String::new();
        for (stage, artifact) in self.artifacts.iter() {
          let color = artifact.text_color;
          let stage = stage.color(color).bold();
          let output = artifact.debug_output.color(color);
          write!(&mut buf, "{}: {}\n", stage, output).unwrap();
        }
        if buf == "" { None } else { Some(buf) }
      },
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
      "ast_reducing" => "red",
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
  fn execute_pipeline(&mut self, _input: &str, _eval_options: &EvalOptions) -> FinishedComputation {
    FinishedComputation { artifacts: vec![], text_output: Err(format!("Execution pipeline not done")) }
  }

  fn get_language_name(&self) -> String;
  fn get_source_file_suffix(&self) -> String;
  fn get_passes(&self) -> Vec<String> {
    vec![]
  }
  fn handle_custom_interpreter_directives(&mut self, _commands: &Vec<&str>) -> Option<String> {
    None
  }
  fn custom_interpreter_directives_help(&self) -> String {
    format!(">> No custom interpreter directives specified <<")
  }
}

/* a pass_chain function signature looks like: 
 * fn(&mut ProgrammingLanguageInterface, A, Option<&mut DebugHandler>) -> Result<B, String>
 *
 * TODO use some kind of failure-handling library to make this better
 */

#[macro_export]
macro_rules! pass_chain {
  ($state:expr, $options:expr; $($pass:path), *) => {
    |text_input| {
      let mut comp = UnfinishedComputation::default();
      pass_chain_helper! { ($state, comp, $options); text_input $(, $pass)* }
    }
  };
}

#[macro_export]
macro_rules! pass_chain_helper {
  (($state:expr, $comp:expr, $options:expr); $input:expr, $pass:path $(, $rest:path)*) => {
    {
      let pass_name = stringify!($pass);
      let output = {
        let ref debug_set = $options.debug_passes;
        let debug_handle: Option<&mut UnfinishedComputation> = if debug_set.contains(pass_name) {
          Some(&mut $comp)
        } else {
          None
        };
        $pass($state, $input, debug_handle)
      };
      match output {
        Ok(result) => pass_chain_helper! { ($state, $comp, $options); result $(, $rest)* },
        Err(err) => {
          $comp.output(Err(format!("Pass {} failed with {:?}", pass_name, err)))
        }
      }
    }
  };
  // Done
  (($state:expr, $comp:expr, $options:expr); $final_output:expr) => {
    {
      let final_output: FinishedComputation = $comp.finish(Ok($final_output));
      final_output
    }
  };
}
