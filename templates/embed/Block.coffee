class Block
  constructor: (@element, @articleId) ->
    @bindEvents()
    @_key = @element.id = @generateKey()
    @comments = []
    @indicator = new Indicator(this)

  id: ->
    @_key

  bindEvents: ->
    @element.addEventListener 'mouseenter', =>
      @indicator.show()
    @element.addEventListener 'mouseleave', =>
      @indicator.hide()

  insert: ->
    @parent().insertBefore(@indicator.element, @element)

  content: ->
    (@element.innerText || @element.textContent || '').replace(/[^a-z\. ]+/gi, '')

  generateKey: ->
    key = ""
    len = 6
    content = @content()
    if content and content.length > 1
      lines = @getSentences(content)
      if lines.length
        first = @cleanArray(lines[0].replace(/[\s\s]+/gi, ' ').split(' ')).slice(0, (len/2))
        last = @cleanArray(lines[lines.length-1].replace(/[\s\s]+/gi, ' ').split(' ')).slice(0, (len/2))
        k = first.concat(last)
        max = if k.length > len then len else k.length
        key += k[i].substring(0, 1) for i in [0..max-1]
    return key

  getSentences: (el) ->
    html = if (typeof el is "string") then el else el.innerHTML
    mrsList = "Mr,Ms,Mrs,Miss,Msr,Dr,Gov,Pres,Sen,Prof,Gen,Rep,St,Messrs,Col,Sr,Jf,Ph,Sgt,Mgr,Fr,Rev,No,Jr,Snr"
    topList = "A,B,C,D,E,F,G,H,I,J,K,L,M,m,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,etc,oz,cf,viz,sc,ca,Ave,St"
    geoList = "Calif,Mass,Penn,AK,AL,AR,AS,AZ,CA,CO,CT,DC,DE,FL,FM,GA,GU,HI,IA,ID,IL,IN,KS,KY,LA,MA,MD,ME,MH,MI,MN,MO,MP,MS,MT,NC,ND,NE,NH,NJ,NM,NV,NY,OH,OK,OR,PA,PR,PW,RI,SC,SD,TN,TX,UT,VA,VI,VT,WA,WI,WV,WY,AE,AA,AP,NYC,GB,IRL,IE,UK,GB,FR"
    numList = "0,1,2,3,4,5,6,7,8,9"
    webList = "aero,asia,biz,cat,com,coop,edu,gov,info,int,jobs,mil,mobi,museum,name,net,org,pro,tel,travel,xxx"
    extList = "www"
    d = "__DOT__"
    list = (topList+","+geoList+","+numList+","+extList).split(",")
    len  = list.length

    html = html.replace(new RegExp((" "+item+"\\."), "g"), (" "+item+d)) for item in list

    list = (mrsList+","+numList).split(",")
    html = html.replace(new RegExp((item+"\\."), "g"), (item+d)) for item in list

    list = (webList).split(",")
    html = html.replace(new RegExp(("\\."+item), "g"), (d+item)) for item in list

    lines = @cleanArray(html.split('. '))
    return lines

  cleanArray: (array) ->
    array.filter((item) ->
      item and item.replace /\s/g, ""
    )

  parent: () ->
    @element.parentNode

  setComments: (commentData) ->
    @comments = commentData.map (comment) ->
      new Comment(comment)
    @indicator.setCount(@comments.length)

  activate: =>
    Carnival.addClass(@element, 'commenting')
    @element.dispatchEvent(new CustomEvent("commenting", bubbles: true, detail: this))

  deactivate: =>
    Carnival.removeClass(@element, 'commenting')
    @element.dispatchEvent(new CustomEvent("doneCommenting", bubbles: true, detail: this))
