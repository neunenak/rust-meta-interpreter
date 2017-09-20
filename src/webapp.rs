use rocket;

use rocket::response::NamedFile;
use rocket_contrib::Json;

#[get("/")]
fn index() -> Result<NamedFile, ()> {
  NamedFile::open("static/index.html").map_err(|_| ())
}

#[derive(Serialize, Deserialize)]
struct Input {
  source: String,
}

#[derive(Serialize, Deserialize)]
struct Output {
  text: String,
}

#[post("/input", format = "application/json", data = "<input>")]
fn interpreter_input(input: Json<Input>) -> Json<Output> {
  let output = Output { text: "test interpreter output".to_string() };
  Json(output)
}

pub fn web_main() {
  rocket::ignite().mount("/", routes![index, interpreter_input]).launch();
}
