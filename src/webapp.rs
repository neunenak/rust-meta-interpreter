use rocket;
use rocket::response::NamedFile;
use rocket_contrib::Json;
use schala_lang;
use language::{ProgrammingLanguageInterface, EvalOptions};

#[get("/")]
fn index() -> Result<NamedFile, ()> {
  NamedFile::open("static/index.html").map_err(|_| ())
}

#[get("/bundle.js")]
fn js_bundle() -> Result<NamedFile, ()> {
  NamedFile::open("static/bundle.js").map_err(|_| ())
}

#[derive(Debug, Serialize, Deserialize)]
struct Input {
  source: String,
}

#[derive(Serialize, Deserialize)]
struct Output {
  text: String,
}

#[post("/input", format = "application/json", data = "<input>")]
fn interpreter_input(input: Json<Input>) -> Json<Output> {
  let mut schala = schala_lang::Schala::new();
  let code_output = schala.evaluate_in_repl(&input.source, &EvalOptions::default());
  Json(Output { text: code_output.to_string() })
}

pub fn web_main() {
  rocket::ignite().mount("/", routes![index, js_bundle, interpreter_input]).launch();
}
