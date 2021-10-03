
switch ReactDOM.querySelector("#app-root") {
  | Some(root) => ReactDOM.render(<Application />, root)
  | None => ()
}
