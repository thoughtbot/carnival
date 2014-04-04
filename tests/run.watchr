# vim:ft=ruby:
watch( '.*\.hs'          ) { system("yesod test") }
watch( 'carnival\.cabal' ) { system("yesod test") }
