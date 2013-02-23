define ->

  padZeros: (n, z=2) ->
    result = n
    len = ((String) n).length
    while len < z
      result = "0" + result
      len++
    result

  dateDiffToString: (diff) ->
    totalMins = parseInt (diff / 60000), 10
    hours = parseInt (totalMins / 60), 10
    mins = totalMins % 60
    (@padZeros hours) + ":" + (@padZeros mins)

  dateDiffSince: (date) ->
    now = new Date()
    @dateDiffToString (now - date)
