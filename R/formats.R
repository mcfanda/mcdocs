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
