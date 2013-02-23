define ['components/flight/lib/component', 'ajax'], (defineComponent, ajax) ->
  component = ->

    @newTask = (name) ->
      ajax.post "/tasks", { name: name }, (err, data) =>
        if err?
          alert err
          return
        @$node.find("input").val ""
        @trigger "#tasks", "newTask", html: data

    @after "initialize", ->
      @$node.keydown (e) =>
        if e.which == 13
          @newTask @$node.find("input").val()

  defineComponent component