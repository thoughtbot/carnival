class CommentForm
  constructor: (@thread) ->
    @createElement()
    @bindEvents()

  createElement: ->
    @element = document.createElement('li')
    @element.id = 'comment-form'
    @element.className = 'carnival-comment-form'
    @element.innerHTML = """
      <a class='carnival-create'><span>+</span> Leave a comment</a>
      <form>
        <a class='carnival-cancel'>✖</a>
        <div class='carnival-author'><img src=''><span></span></div>
        <div class='carnival-validation-error'></div>
        <textarea placeholder='Type here&hellip;' class='carnival-body'></textarea>
        <input type='submit' value='Comment'>
      </form>
    """

  body: ->
    @element.querySelector('.carnival-body').value

  resizeTextarea: ->
    if @scrollHeight > @clientHeight
      @style.height = @scrollHeight + "px"

  bindEvents: ->
    @element.querySelector('textarea').addEventListener 'keyup', @resizeTextarea
    @element.addEventListener 'submit', (event) =>
      event.preventDefault()
      event.stopPropagation()
      @saveComment()
    @element.querySelector('a.carnival-create').addEventListener 'click', (event) =>
      event.preventDefault()
      event.stopPropagation()
      if Carnival.isLoggedIn()
        @showCommentForm()
      else
        Carnival.login()
    @element.querySelector('a.carnival-cancel').addEventListener 'click', (event) =>
      event.preventDefault()
      event.stopPropagation()
      @hideCommentForm()
    @element.querySelector('textarea').addEventListener 'keydown', (event) =>
      if event.keyCode == 13 and (event.metaKey or event.ctrlKey)
        @saveComment()
    document.addEventListener 'hasLoggedIn', =>
      @showCommentForm()

  hasBody: ->
    @body().replace(/\s/g, '').length > 0

  showCommentForm: ->
    @element.querySelector('a').style.display = 'none'
    @element.querySelector('form').style.display = 'block'
    @element.querySelector('.carnival-author span').innerHTML = Carnival.userName()
    @element.querySelector('.carnival-author img').src = Carnival.userGravatarUrl()
    @element.querySelector('.carnival-body').focus()

  hideCommentForm: ->
    @element.querySelector('form').style.display = 'none'
    @element.querySelector('a').style.display = 'block'

  saveComment: ->
    error = @element.querySelector('.carnival-validation-error')
    error.innerHTML = ''

    if @hasBody()
      @thread.add(@body())
      @element.querySelector('.carnival-body').value = ''
    else
      error.innerHTML = 'comments cannot be empty'
