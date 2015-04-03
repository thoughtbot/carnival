class Thread
  constructor: (@article) ->
    @createElement()
    @renderCommentForm()
    @active = false

    # A bare interpolation doesn't seem to work. Doing it within a string does.
    # Therefore we have to "cast" it back to a boolean. This is probably a bug
    # in Text.Coffee
    @branded = '%{branded}' == 'true'

  id: ->
    @currentBlock.id() + '-comments'

  createElement: =>
    @element = document.createElement('ol')

  setupElement: ->
    @element.id = @id()
    @element.setAttribute('data-comments-for', @currentBlock.id())
    @element.className = 'carnival-comments'

  displayForBlock: (@currentBlock) ->
    @setupElement()
    @insertCommentsIntoDom()
    @renderCommentForm()
    @renderBranding()
    @article.container.insertBefore(@element, @article.container.lastChild)
    @element.style.top = @currentBlock.top() + 'px'

  comments: ->
    @currentBlock.comments

  insertCommentsIntoDom: ->
    @removeChildrenBySelector('li.carnival-comment')
    for comment in @comments()
      comment.insert(@element)

  renderCommentForm: ->
    @commentForm ?= new CommentForm(this)
    @element.appendChild(@commentForm.element)

  renderBranding: ->
    if @branded
      @removeChildrenBySelector('li.carnival-branding')

      el = document.createElement('li')
      el.classList.add('carnival-comment')
      el.classList.add('carnival-branding')
      el.innerHTML = 'Comments powered by <a href=@{RootR}>Carnival</a>.'

      @element.appendChild(el)

  add: (body) ->
    Carnival.post(
      '@{CommentsR siteId}',
      @commentHash(body),
      (response) =>
        comment = new Comment(response.comment)
        @comments().push(comment)
        @currentBlock.indicator.setCount(@comments().length)
        comment.insert(@element)
        @renderCommentForm()
        @renderBranding()
    )

  commentHash: (body) ->
    {
      article_url: @article.id,
      article_title: @article.title,
      article_author: @article.author,
      thread: @currentBlock.id(),
      body: body
    }

  removeChildrenBySelector: (selector) ->
    children = @element.querySelectorAll(selector)

    [].slice.call(children).map((comment) =>
      @element.removeChild(comment)
    )
