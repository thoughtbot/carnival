class Article
  constructor: (@element) ->
    @id = location.pathname
    @title = document.title
    @author = CarnivalOptions.article_author
    @container = @createContainer()
    @thread = new Thread(this)
    @fetchComments()
    @bindEvents()

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
      Carnival.addClass(@element, 'commenting')
      @thread.displayForBlock(event.detail)
    @element.addEventListener 'doneCommenting', =>
      if @element.querySelectorAll('.commenting').length is 0
        Carnival.removeClass(@element, 'commenting')

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
