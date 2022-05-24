#' format an option
#'
#' @param opt a text

#' @export
opt<-function(opt) {
  paste0('<span class="option">',opt,'</span>')
}

#' format the name of the module
#' @export
modulename<-function() paste0('<span class="modulename">',.GlobalEnv$MODULE_NAME,'</span>')

#' make a link to a data file
#' @export

datafile<-function(name,file) {
  if (length(grep(":/",file,fixed = T))==0)
    file<-paste0(DATALINK,"/",file)
  paste0('[',name,'](',file,')')
}

#' format keywords of the page
#' @export

keywords<-function(key) {
  span<-'<span class="keywords"> <span class="keytitle"> keywords </span>'
  paste(span,key,"</span>")
}

#' format the version of the module
#' @param ver version to print
#' @export

version<-function(ver) {
  paste('<span class="version"> <span class="versiontitle">',MODULE_NAME,' version ≥ </span> ',ver,' </span>')
}

#' insert a picture
#' @param name the path to the picture
#' @export
pic<-function(path) paste('<img src="',path,'" class="img-responsive" alt="">')


### internal functions
#' @export
get_files<-function(path=".",pattern=".Rmd") {
  lf<-list.files(path=path,pattern = pattern,full.names = F)
  files<-list()
  for (f in lf) {
    name<-gsub(".Rmd","",f)
    record<-rmarkdown::yaml_front_matter(f)
    record$filename<-name
    files[[name]]<-record
  }
  files
}

#' @export
get_pages<-function(nickname=NULL,topic=NULL,category=NULL) {

  criteria<-rlist::list.clean(c(nickname=nickname,topic=topic,category=category))
  files<-get_files()
  if (!is.null(criteria)) {
    search<-rlist::list.clean(files,function(a) {
      test<-!(c("nickname","topic") %in% names(a))
      any(test)
    }
    )
    acrit<-paste(names(criteria),paste0("'",criteria,"'"),sep = "==",collapse = " && ")
    acall<-as.call(str2lang(acrit))
    files<-rlist::list.filter(search, eval(acall))

  }
  return(files)
}

#' print a link to pages
#' @param nickname of the file, set in yaml head of the file
#' @param topic of the file, set in yaml head of the file
#' @param category of the file, set in yaml head of the file
#' @export
link_pages<-function(nickname=NULL,topic=NULL,category=NULL) {

  pages<-get_pages(nickname,topic,category)
  a<-""
  for (p in pages) {
    link<-paste0(p$filename,".html")
    a<-paste(a,paste0('<a href="',link,'">',p$title,'</a>'))
  }
  return(a)
}

#' print a html list of pages with links
#' @param nickname of the file, set in yaml head of the file
#' @param topic of the file, set in yaml head of the file
#' @param category of the file, set in yaml head of the file
#' @export
list_pages<-function(nickname=NULL,topic=NULL,category=NULL) {
  pages<-get_pages(nickname,topic,category)
  ul<-'<ul>\n'
  a<-""
  for (p in pages) {
    link<-paste0(p$filename,".html")
    b<-paste0('<li><a href="',link,'">',p$title,'</a></li>\n')
    a<-paste(a,b)
  }
  a<-paste(ul,a,'</ul>\n')

  return(a)
}

#' print a html list of pages of category example
#' @param topic of the file, set in yaml head of the file
#' @export

include_examples<-function(topic=NULL)  {
  return(list_pages(topic=topic,category = "example"))
}

#' print a html list of pages of category details
#' @param topic of the file, set in yaml head of the file
#' @export

include_details<-function(topic)  {
  return(list_pages(topic=topic,category = "details"))
}

#' print a html paragraph for issues
#' @param topic of the file, set in yaml head of the file
#' @export

issues<-function() {
  a<-'<h1>Comments?</h1>\n'
  a<-paste0(a,' <p>Got comments, issues or spotted a bug? Please open an issue on
      <a href="',MODULE_LINK,'/issues ">
      ',MODULE_NAME,' at github</a> or <a href="mailto:',MODULE_EMAIL,'">send me an email</a></p>
  ')
  return(a)

}

#' print a link to some topic
#' @param topic of the file, set in yaml head of the file
#' @export

backto<-function(topic) {
  a<-'<p class="return"> Return to main help page: '
  p<-get_pages(topic=topic,category = "help")[[1]]
  link<-paste0(p$filename,".html")
  b<-paste0('<a href="',link,'">',p$title,'</a>')
  d<-paste(a,b,"</p>")
  return(d)

}


get_options<-function(com,optnames) {

  output<-list()
  file<-paste0(MODULE_FOLDER,"/jamovi/",com,".a.yaml")
  obj<<-yaml::read_yaml(file)
  options<-obj$options
  for (optname in optnames) {
      res<-rlist::list.find(options,name==optname)
      output[[length(output)+1]]<-res
  }
  output
}

format_options<-function(com,optnames) {

  opts<-get_options(com,optnames)
  for (opt in opts) {
    opt<-opt[[1]]
    desc<-if (hasName(opt$description,"ui")) opt$description$ui else opt$description
    cat("* ",opt(opt$title)," ",desc,"\n")
  }

}
