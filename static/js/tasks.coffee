define ['components/flight/lib/component', 'decorate', 'task'], (defineComponent, decorator, taskC) ->
  component = ->

    @sumUpSessions = ->
      result = 0
      for selector in [".tasktime:visible", ".running:visible"]
        $(selector).each ->
          val = $(@).text().trim()
          split = val.split ":"
          result += 60 * ((Number) split[0]) + ((Number) split[1])
      result

    @addTask = (e, task) ->
      @$node.append task.html
      taskId = $(task.html).find(".task").attr("id").replace "task-", ""
      taskElement = $("#task-" + taskId)
      decorator.decorateTask taskElement
      taskC.attachTo taskElement, taskId: taskId

    @calculateTotal = ->
      @trigger $("#totalval"), "setTotal", secs: @sumUpSessions()

    @replaceTask = (e, task) ->
      element = @$node.find("#task-" + task.taskId)
      expanded = element.find(".expand").hasClass "active"
      newElement = if task.element.hasClass ".task"
                     task.element
                   else
                     task.element.find ".task"
      element.replaceWith newElement
      decorator.decorateTask newElement
      taskC.attachTo newElement, taskId: task.taskId
      @trigger "totalChanged"
      @trigger newElement, "toggleExpand" if expanded

    @summaryUrl = ->
      query = ""
      $(".task:visible").each ->
        id = $(@).attr("id").replace("task-", "")
        query += "-#{id}"
      query = query.substring 1
      "/summary?tasks=#{query}"

    @getSummary = ->
      window.location = @summaryUrl()

    @getSummaryJson = ->
      url = @summaryUrl()
      $.ajax url,
        dataType: "json"
        success: (data) ->
          window.prompt "Copy that:", JSON.stringify(data)

    @after "initialize", ->
      @calculateTotal()
      @on "totalChanged", @calculateTotal
      @on "newTask", @addTask
      @on "replaceTask", @replaceTask
      @on "getSummary", @getSummary
      @on "getSummaryJson", @getSummaryJson

  defineComponent component