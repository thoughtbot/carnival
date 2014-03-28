class Article
  constructor: (@element) ->
    @id = @element.getAttribute('data-url').replace(/\//g, '')
    @fetchComments()
    @bindEvents()

  createBlocks: ->
    @blocks = [].slice.call(@element.querySelectorAll(CarnivalOptions.block_selector)).map (blockElement, index) =>
      new Block(blockElement, @id, index)

  insertBlocksIntoDom: ->
    for block in @blocks
      block.commentData = @commentData.filter((comment) =>
        comment.thread is block.id()
      )
      block.insert(@element)

  bindEvents: ->
    @element.addEventListener 'commenting', =>
      Carnival.addClass(@element, 'commenting')
    @element.addEventListener 'doneCommenting', =>
      if @element.querySelectorAll('.commenting').length is 0
        Carnival.removeClass(@element, 'commenting')

  fetchComments: ->
    Carnival.get('/comments?article=' + @id, (data) =>
      @commentData = data.comments
      @createBlocks()
      @insertBlocksIntoDom()
    )
