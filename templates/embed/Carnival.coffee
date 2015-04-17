class Carnival
  constructor: () ->
    if(CarnivalOptions.enabled)
      @elements = document.querySelectorAll(CarnivalOptions.article_selector)
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
    request.withCredentials = true
    request.open('GET', url, true)
    request.setRequestHeader('Content-Type', 'application/json')
    request.onload = () ->
      if request.status >= 200 and request.status < 400
        callback(JSON.parse(request.responseText))
    request.send()

  @post: (url, data, callback) ->
    request = new XMLHttpRequest()
    request.withCredentials = true
    request.open('POST', url, true)
    request.setRequestHeader('Content-Type', 'application/json')
    request.onload = () ->
      if request.status >= 200 and request.status < 400
        callback(JSON.parse(@responseText))
    request.send(JSON.stringify(data))

  @userName: ->
    Carnival.user.name

  @userGravatarUrl: ->
    Carnival.user.gravatar_url

  @isLoggedIn: ->
    @getUser()
    if @user?
      return true
    else
      return false

  @getUser: ->
    request = new XMLHttpRequest
    request.withCredentials = true
    request.open('GET', '@{UserR}', false)
    request.setRequestHeader('Accept', 'application/json')
    request.send()
    if request.status is 200
      @user = JSON.parse(request.responseText).user

  @hasLoggedIn: (event) =>
    if event.origin != '%{root}'
      return
    @loginWindow.close()
    @getUser()
    document.dispatchEvent(new CustomEvent("hasLoggedIn", bubbles: true))

  @login: ->
    width = 600
    height = 600
    left = (screen.width/2)-(width/2)
    top = (screen.height/2)-(height/2)
    @loginWindow = window.open(
      '@{AuthR LoginR}?dest=@{RootR}',
      'carnivalLogin',
      'height='+height+',width='+width+',top='+top+',left='+left+',menubar=no'
    )
    window.addEventListener 'message', @hasLoggedIn
