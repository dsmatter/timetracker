define ['components/flight/lib/component', 'decorate'], (defineComponent, decorator) ->
  component = ->

    @secondsToDurationString = (n) ->
      hours = parseInt(n / 60)
      mins = n % 60
      sHours = if hours < 10 then "0" + hours else hours.toString()
      sMins = if mins < 10 then "0" + mins else mins.toString()
      "#{sHours}:#{sMins}"

    @setTotal = (e, total) ->
      @$node.find("#totalval").text (@secondsToDurationString total.secs)

    @after "initialize", ->
      @$node.find(".summary").click =>
        @trigger "#tasks", "getSummary"
      @$node.find(".summary-json").click =>
        @trigger "#tasks", "getSummaryJson"

      @on "setTotal", @setTotal

  defineComponent component