define ['components/flight/lib/component', 'tasks'], (defineComponent, tasks) ->
  component = ->

    @isSubstring = (h, n) ->
      h.toUpperCase().indexOf(n.toUpperCase()) >= 0

    @taskMatches = (task, string) ->
      (@isSubstring task.find(".taskname").text(), string) or
        (@isSubstring task.find(".tags").text(), string)

    @search = (e, filter) ->
      return unless filter? and filter.text?
      self = @
      $(".task").each ->
        if self.taskMatches $(@), filter.text
          $(@).removeClass "hidden"
        else
          $(@).addClass "hidden"
      @trigger "#tasks", "totalChanged"

    @after "initialize", ->
      inputElement = @$node.find("input")
      inputElement.keyup =>
        @trigger "search", text: inputElement.val()

      @on "search", @search

  defineComponent component