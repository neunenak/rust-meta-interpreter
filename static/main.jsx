const React = require("react");
const ReactDOM = require("react-dom");
const superagent = require("superagent");

const serverAddress = "http://localhost:8000";

class CodeArea extends React.Component {
  constructor(props) {
    super(props);
    this.state = {value: "", lastOutput: null};
    this.handleChange = this.handleChange.bind(this);
    this.submit = this.submit.bind(this);
  }

  handleChange(event) {
    this.setState({value: event.target.value});
  }

  submit(event) {
    console.log("Event", this.state.value);
    const source = this.state.value;

    superagent.post(`${serverAddress}/input`)
      .send({ source })
      .set("accept", "json")
      .end((error, response) => {
        if (response) {
          console.log("Resp", response);
          this.setState({lastOutput: response.body.text})
        } else {
          console.error("Error: ", error);
        }
      });
  }

  renderOutput() {
    if (!this.state.lastOutput) {
      return null;
    }
    return <textarea readOnly value={ this.state.lastOutput } />;
  }

  render() {
    return (<div className="CodeArea">
      <div className="input">
        <textarea value={ this.state.value } onChange={this.handleChange}>
        </textarea>
        <button onClick={ this.submit }>Run!</button>
      </div>
      <div className="output">
        { this.renderOutput() }
      </div>
    </div>);
  }
}

const main = (<div>
  <h1>Schala web input</h1>
  <p>Write your source code here</p>
  <CodeArea/>
</div>);

const rootDom = document.getElementById("main");
ReactDOM.render(main, rootDom);
