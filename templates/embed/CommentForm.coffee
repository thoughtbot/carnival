class CommentForm
  constructor: (@thread) ->
    @createElement()
    @bindEvents()

  createElement: ->
    @element = document.createElement('li')
    @element.id = 'comment-form'
    @element.className = 'comment-form'
    @element.innerHTML = "<a class='create'><span>+</span> Leave a comment</a><form><a class='cancel'>✖</a><div class='author'><img src=''><span></span></div><textarea placeholder='Type here&hellip;' class='body'></textarea><input type='submit' value='Comment'></form>"

  body: ->
    @element.querySelector('.body').value

  resizeTextarea: ->
    if @scrollHeight > @clientHeight
      @style.height = @scrollHeight + "px"

  bindEvents: ->
    @element.querySelector('textarea').addEventListener 'keyup', @resizeTextarea
    @element.addEventListener 'submit', (event) =>
      event.preventDefault()
      event.stopPropagation()
      @saveComment()
    @element.querySelector('a.create').addEventListener 'click', (event) =>
      event.preventDefault()
      event.stopPropagation()
      if Carnival.isLoggedIn()
        @showCommentForm()
      else
        Carnival.login()
    @element.querySelector('a.cancel').addEventListener 'click', (event) =>
      event.preventDefault()
      event.stopPropagation()
      @hideCommentForm()
    @element.querySelector('textarea').addEventListener 'keydown', (event) =>
      if event.keyCode == 13 and event.metaKey
        @saveComment()
    document.addEventListener 'hasLoggedIn', =>
      @showCommentForm()

  hasBody: ->
    @body().replace(/\s/g, '').length > 4

  showCommentForm: ->
    @element.querySelector('a').style.display = 'none'
    @element.querySelector('form').style.display = 'block'
    @element.querySelector('.author span').innerHTML = Carnival.userName()
    @element.querySelector('.author img').src = Carnival.userGravatarUrl()
    @element.querySelector('.body').focus()

  hideCommentForm: ->
    @element.querySelector('form').style.display = 'none'
    @element.querySelector('a').style.display = 'block'

  saveComment: ->
    if @hasBody()
      @thread.add(@body())
      @element.querySelector('.body').value = ''
