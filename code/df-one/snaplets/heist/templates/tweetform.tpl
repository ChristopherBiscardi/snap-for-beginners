<apply template="base">
  <dfForm action="/tweet">
    <dfChildErrorList ref="" />

    <dfLabel ref="username">Username: </dfLabel>
    <dfInputText ref="username" />
    <br>

    <dfLabel ref="timestamp">Timestamp: </dfLabel>
    <dfInput ref="timestamp" type="number" min="0" step="1" pattern="\d+" />
    <br>

    <dfLabel ref="content">Content: </dfLabel>
    <dfInputTextArea ref="content" />
    <br>

    <dfInputSubmit value="Submit" />
  </dfForm>
</apply>