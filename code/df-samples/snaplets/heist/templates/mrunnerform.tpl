<apply template="base">
  <dfForm action="/mrunner">
    <dfChildErrorList ref="" />

    <dfLabel ref="name">Name: </dfLabel>
    <dfInputText ref="name" />
    <br>

    <dfLabel ref="isrunner">Are you a Runner? </dfLabel>
    <dfInputCheckbox ref="isrunner"/>
    <br>

    <dfInputSubmit value="Submit" />
  </dfForm>
</apply>