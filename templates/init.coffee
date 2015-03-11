window.Carnival =
  init: (options) ->
    Carnival.setOptions(options)

    xmlhttp = new XMLHttpRequest();
    xmlhttp.onreadystatechange = () =>
      if xmlhttp.readyState == 4 && xmlhttp.status == 200
        n = document.createElement('div')
        n.innerHTML = xmlhttp.responseText
        document.body.appendChild(n)

        Carnival.replaceScriptsRecurse(n)

    xmlhttp.open('GET', '@{EmbedR siteId}?t=' + Math.random(), true);
    xmlhttp.send();

  setOptions: (options) ->
    window.CarnivalOptions = options || {}

    defaults =
      enabled: true
      article_author: ''
      article_selector: 'article'
      block_selector: ':scope > p, :scope > pre'
      onNewComment: (comment) ->

    for property of defaults
      unless property of CarnivalOptions
        CarnivalOptions[property] = defaults[property]

  replaceScriptsRecurse: (node) ->
    if Carnival.isScriptNode(node)
      script = document.createElement('script')
      script.src = node.src

      node.parentNode.replaceChild(script, node)
    else
      Carnival.replaceScriptsRecurse(child) for child in node.childNodes

    node;

  isScriptNode: (node) ->
    node.getAttribute and node.tagName == 'SCRIPT'
