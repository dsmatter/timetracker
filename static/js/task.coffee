define ['components/flight/lib/component', 'ajax'], (defineComponent, ajax) ->
  component = ->

    @delete = ->
      ajax.delete "/tasks/" + @attr.taskId, "", (err, data) =>
        if err?
          alert err
          return
        @$node.fadeOut =>
          @trigger "#tasks", "totalChanged"
          @$node.remove()

    @nameWithTags = ->
      result = @$node.find(".taskname").text()
      tags = @$node.find(".tags").text()
      if tags != ""
        return result + " " + tags
      else
        return result

    @enterEditMode = ->
      nameElement = @$node.find ".taskname"
      oldName = @nameWithTags()
      nameElement.html('<input type="text" value="' + oldName + '" />')
      inputElement = nameElement.find "input"
      inputElement.focus()
      inputElement.keydown (e) =>
        if e.which == 13
          @exitEditMode inputElement

    @exitEditMode = (input) ->
      name = input.val()
      ajax.put "/tasks/" + @attr.taskId, name: name, (err, data) =>
        if err?
          alert err
          return
        newName = $(data).find ".taskname"
        newTags = $(data).find ".tags"
        @$node.find(".taskname").replaceWith newName
        @$node.find(".tags").replaceWith newTags

    @after "initialize", ->
      @$node.find(".delete").click =>
        @trigger "delete"
      @$node.find(".edit").click =>
        @trigger "edit"

      @on "delete", @delete
      @on "edit", @enterEditMode

  defineComponent component