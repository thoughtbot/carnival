class Article
  constructor: (@element) ->
    @id = @element.getAttribute('data-url').replace(/\//g, '')
    @blocks = @createBlocks()
    @insertIntoDom()
    @bindEvents()

  createBlocks: ->
    [].slice.call(@element.querySelectorAll(CarnivalOptions.block_selector)).map (blockElement, index) =>
      new Block(blockElement, @id, index)

  insertIntoDom: ->
    for block in @blocks
      block.insert(@element)

  bindEvents: ->
    @element.addEventListener 'commenting', =>
      Carnival.addClass(@element, 'commenting')
    @element.addEventListener 'doneCommenting', =>
      if @element.querySelectorAll('.commenting').length is 0
        Carnival.removeClass(@element, 'commenting')
