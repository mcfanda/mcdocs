#' create a foldable section with title
#' @param title a title
#' @param comment (optional) comment
#' @export

foldable_title<-function(title,comment=NULL) {
  a<-'<button type="button" class="collapsible">'
  a<- paste(a,fa("expand-alt", fill = "steelblue"))
  a<- paste(a,'<span class="colltitle">',title,"</span>")
  a<- paste(a,'<span class="collinfo">click to read</span>')
  if (!is.null(comment))
    a<-paste(a,' <span class="collinfo">',comment,'</span>')
  a<-paste(a,'</button>')
  a<-paste(a,'<div class="collapsiblecontent">')
  a
}

#' format the version of the module
#' @param ver version to print
#' @export

version<-function(ver) {
  paste('<div class="version"> <p>',ver,' </p></div><div style="clear:both"></div>')
}

#' format keywords of the page
#' @export

keywords<-function(key) {

  if (exists("WHERE"))
    if (WHERE!="html") return("")


  span<-'<span class="keywords"> <span class="keytitle"> keywords </span>'
  paste(span,key,"</span>")
}

#' format tooltips
#' @export

tooltip<-function(key,value) {
  paste0("<span class='tooltip'>",key," <span class='tooltiptext'>",value,"</span> </span>")
}


#' export a reference for bookdwon
#' @export

sec<-function(key) {

  paste0("Section \\@ref(",key,")")
}

#' export a reference for bookdwon
#' @export

cap<-function(key) {

  paste0("Chapter \\@ref(",key,")")
}

#' format oldnames of models
#' @export

oldnames<-function(key) {

  if (exists("WHERE"))
      if (WHERE!="html") {
        x<-paste(' \\begin{flushright}','AKA:',key,' \\end{flushright}')
        return(x)
      }
  span<-'<span class="oldnames"> <span class="oldnamestitle"> AKA </span>'
  paste(span,key,"</span>")
}

#' format an option
#'
#' @param name a text

#' @export
opt<-function(name) {
  paste0('<span class="option">',name,'</span>')
}

#' format panel name
#'
#' @param name a text

#' @export
opt_panel<-function(name) {
  paste0('<span class="panel" > <i class="fa fa-chevron-down"></i> | ',name,'</span> panel')
}

#' format jamovi label name
#'
#' @param name a text

#' @export
opt_label<-function(name) {
  paste0('<span class="option" > <b>', name,' </b></span>')
}

#' format jamovi title name
#'
#' @param name a text

#' @export
opt_title<-function(name) {
  paste0('<span class="option_title" > ', name,' </span>')
}

#' format jamovi mode label name
#'
#' @param name a text

#' @export
opt_mode<-function(name) {
  paste0('<span class="option_mode" > ', name,' </span>')
}


#' format a table name
#'
#' @param name a text

#' @export
tab<-function(name) {
  paste0('<span class="tablename">',name,'</span>')
}

#' format the name of the module
#' @export
modulename<-function() paste0('<span class="modulename">',.GlobalEnv$MODULE_NAME,'</span>')


#' make a link external stuff
#' @export

ext_url<-function(title,href) {
  paste0("<a href='",href,"' target='_blank'>",title,"</a>")
}

#' make a head for filename of a code chunk
#' @export

filehead<-function(title,ref=NULL,ver=NULL) {

  if (!is.null(ref))
    link<-paste0("(<a href='",ref,ver,"/",title,"' target='_blank'>Working version</a>)")
  else
    link<-""

  paste0('<div class="filename"> <span class="filename">File: ',title,'  ',link,'</span></div>')
}
