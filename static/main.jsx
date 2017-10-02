const React = require("react");
const ReactDOM = require("react-dom");
const request = require("request");

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

    const options = {
      url: `${serverAddress}/input`,
      json: true,
      body: { source }
    };
    request.post(options, (error, response, body) => {
      this.setState({lastOutput: body.text})
    });
  }

  renderOutput() {
    if (!this.state.lastOutput) {
      return null;
    }
    return <textarea readOnly value={ this.state.lastOutput } />;
  }

  render() {
    return (<div>
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
