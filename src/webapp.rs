#[get("/")]
fn index() -> &'static str {
  "rocket test"
}

fn web_main() {
  rocket::ignite().mount("/", routes![index]).launch();
}
