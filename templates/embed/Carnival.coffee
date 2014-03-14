class Carnival
  constructor: () ->
    @setOptionsFromDefaults()
    @elements = document.getElementsByTagName(CarnivalOptions.article_tag)
    @articles = [].slice.call(@elements).map (articleElement) ->
      new Article(articleElement)

  @defaults:
    article_tag: 'article'
    block_selector: ':scope > p, :scope > pre'

  setOptionsFromDefaults: ->
    for property of Carnival.defaults
      unless property of CarnivalOptions
        CarnivalOptions[property] = Carnival.defaults[property]

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
    request.open('GET', 'http://' + CarnivalOptions.host + url, true)
    request.onload = () ->
      if request.status >= 200 and request.status < 400
        callback(JSON.parse(request.responseText))
    request.send()

  @post: (url, data, callback) ->
    request = new XMLHttpRequest()
    request.open('POST', 'http://' + CarnivalOptions.host + url, true)
    request.onload = () ->
      if request.status >= 200 and request.status < 400
        callback()
    request.send(JSON.stringify(data))
