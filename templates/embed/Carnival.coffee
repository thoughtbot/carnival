class Carnival
  constructor: (@elements) ->
    @articles = [].slice.call(@elements).map (articleElement) ->
      new Article(articleElement)

  @hasClass: (elem, className) ->
    new RegExp(' ' + className + ' ').test(' ' + elem.className + ' ')

  @addClass: (elem, className) ->
    if(!@hasClass(elem, className))
      elem.className += ' ' + className

  @removeClass: (elem, className) ->
    newClass = ' ' + elem.className.replace( /[\t\r\n]/g, ' ') + ' '
    if (@hasClass(elem, className))
      while (newClass.indexOf(' ' + className + ' ') >= 0 )
        newClass = newClass.replace(' ' + className + ' ', ' ')
      elem.className = newClass.replace(/^\s+|\s+$/g, '')

  @get: (url, callback) ->
    request = new XMLHttpRequest
    request.open('GET', 'http://localhost:3000' + url, true)
    request.onload = () ->
      if request.status >= 200 and request.status < 400
        callback(JSON.parse(request.responseText))
    request.send()

  @post: (url, data, callback) ->
    request = new XMLHttpRequest()
    request.open('POST', 'http://localhost:3000' + url, true)
    request.onload = () ->
      if request.status >= 200 and request.status < 400
        callback()
    request.send(JSON.stringify(data))
