class Article
  constructor: (@element) ->
    @id = location.pathname.replace(/\//g, '')
    @title = document.title
    @author = CarnivalOptions.article_author
    @container = @createContainer()
    @thread = new Thread(this)
    @fetchComments()
    @bindEvents()
    @element.style.position = 'relative'

  createBlocks: ->
    @blocks = [].slice.call(@element.querySelectorAll(CarnivalOptions.block_selector)).map (blockElement, index) =>
      new Block(blockElement, @id)

  insertBlocksIntoDom: ->
    for block in @blocks
      block.setComments(@commentData.filter((comment) =>
        comment.thread is block.id()
      ))
      block.insert()

  bindEvents: ->
    @element.addEventListener 'commenting', (event) =>
      unless @element.classList.contains('commenting')
        Carnival.addClass(@element, 'commenting')
        @shiftArticle()

      @thread.displayForBlock(event.detail)
    @element.addEventListener 'doneCommenting', =>
      if @element.querySelectorAll('.commenting').length is 0
        Carnival.removeClass(@element, 'commenting')
        @restoreArticle()

  fetchComments: ->
    Carnival.get('@{CommentsR siteId}?article=' + @id, (data) =>
      @commentData = data.comments
      @createBlocks()
      @insertBlocksIntoDom()
    )

  createContainer: ->
    container = document.createElement('div')
    container.className = 'carnival'
    @element.insertBefore(container, @element.firstChild)
    container

  shiftArticle: =>
    if @wideScreen()
      @prevLeft = @element.style.left
      newLeft = (parseInt(@prevLeft) || 0) - 150
      @element.style.left = newLeft + 'px'

  restoreArticle: =>
    if @wideScreen()
      @element.style.left = @prevLeft

  wideScreen: =>
    window.matchMedia('only screen and (min-width: 640px)').matches
