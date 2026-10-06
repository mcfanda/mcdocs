
#' make a link to a data file
#' @export

datafile<-function(name,file) {
  if (length(grep(":/",file,fixed = T))==0)
    file<-paste0(DATALINK,"/",file)
  paste0('[',name,'](',file,'){target="_blank"}')
}



#' insert a picture
#' @param name the path to the picture
#' @export
#pic<-function(path) paste('<img src="',path,'" class="img-responsive" alt="">')
pic<-function(path) knitr::include_graphics(path)


### internal functions
#' @export
get_files<-function(path=".",pattern=".Rmd") {

  lf<-list.files(path=path,pattern = pattern,full.names = T)
  files<-list()
  for (f in lf) {
    name<-gsub(pattern,"",f)
    record<-rmarkdown::yaml_front_matter(f)
    record$filename<-name
    files[[name]]<-record
  }
  files
}

#' @export
get_pages<-function(nickname=NULL,topic=NULL,category=NULL, path=".") {


  criteria<-rlist::list.clean(c(nickname=nickname,topic=topic,category=category))
  snames<-names(criteria)
  files<-get_files(path)

  if (!is.null(criteria)) {
    search<-rlist::list.clean(files,function(a) {
      test<-!(snames %in% names(a))
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
#' @param path  path where to find the file, default "."
#' @export
link_pages<-function(nickname=NULL,topic=NULL,category=NULL, path=".") {

  pages<-get_pages(nickname,topic,category, path=path)
  a<-""
  for (p in pages) {
   title<-p$title
   if (hasName(p,"linklabel")) title<-p$linklabel
   link<-paste0(p$filename,".html")
   a<-paste(a,paste0('<a href="',link,'">',title,'</a>'))
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
  i<-0
  if (length(pages)==0) return("")

   if (is.null(nickname) && is.null(topic)) {
      ord<-unlist(lapply(pages,function(x) {
         x$topic
      }))
      pages<-pages[order(ord)]
    }


   ord<-unlist(lapply(pages,function(x) {
     i<<-i+1
     if (hasName(x,"order"))
       return(x$order)
     else
      return(i)
   }))

   pages<-pages[order(ord)]


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

include_pages<-function(topic=NULL,category=NULL,text="",title="", level=1)  {

  tag<-paste0(rep("#",level),collapse="")
  text<-paste0(" <div class='adm adm-seealso'>\n",tag," ",title,"\n",
              text,
              list_pages(topic=topic,category =category),"</div>")
  return(text)

  }


#' print a html list of pages of category example
#' @param topic of the file, set in yaml head of the file
#' @export

include_examples<-function(topic=NULL,mute=FALSE,title="", level=1)  {

  tag<-paste0(rep("#",level),collapse="")
  a<-"<p>Some worked out practical examples can be found here</p>"
  if (mute) a<-""
  text<-paste0(" <div class='adm adm-seealso'>\n",tag," ",title," Examples\n",
              a,
              list_pages(topic=topic,category = "example"),"</div>")
  return(text)

  }

#' print a html list of pages of category details
#' @param topic of the file, set in yaml head of the file
#' @export

include_details<-function(topic=NULL,mute=FALSE,title="")  {
  a<-"<p>Some more information about the module specs can be found here</p>"
  if (mute) a<-""
  text<-paste(" <div class='adm adm-seealso'>\n# ",title," Details\n",
              a,
              list_pages(topic=topic,category = "details"),"</div>")
  return(text)
}

#' print a html paragraph for issues
#' @param topic of the file, set in yaml head of the file
#' @export

issues<-function() {
  a<-'<div class="adm adm-warning">\n'
  a<-paste0(a,'# Comments? \n <p>Got comments, issues or spotted a bug? Please open an issue on
        <a href="',MODULE_LINK,'/issues " target="_blank">
      ',MODULE_NAME,' at github</a> or <a href="mailto:',MODULE_EMAIL,'">send me an email</a></p></div>
  ')
  return(a)

}

#' print a link to some topic
#' @param topic of the file, set in yaml head of the file
#' @export

backto<-function(...) {
  topics<-list(...)
  a<-'<div class="adm adm-seealso backto"> <p> Return to main help pages</p> '
  a<-paste(a,"<a class='backto' href='index.html'>Main page</a>")
  for (topic in topics) {
  p<-get_pages(topic=topic,category = "help")
  if (length(p)==0)
      return("")
  p<-p[[1]]
  link<-paste0(p$filename,".html")
  a<-paste0(a,' <a class="backto" href="',link,'">',p$title,'</a>')
  }
  a<-paste(a,"</div>")
  return(a)

}


#' I forgot what it does, but it is useful for extracting help from r packages
#' @param rd an object
#' @export

fixRd<-function(rd) {
#  print(val<-Rdpack::Rdo_locate_core_section(rdo = rd,sec = "\\value"))
  val<-Rdpack::Rdo_locate_core_section(rdo = rd,sec = "\\value")[[1]]$pos
  value<-rd[[val]]
  rvalue<-Rdpack::Rdapply(value,function(r) {
    if(length(grep("$",r,fixed = T))>0)
      return(paste0("`",r,"`"))
    else return(r)
  })
  rdvalue<-Rdpack::char2Rdpiece(value,name = "value")
  Rdpack::Rdo_replace_section(rd,rdvalue)
}


#' list vignettes , if any
#' @export

link_vignettes<-function() {

  folder<-paste0(MODULE_FOLDER,"/vignettes/")
  pages<-get_files(path=folder,pattern = "*.Rmd")
  ul<-'<ul>\n'
  a<-""
  for (p in pages) {
    link<-paste0(p$filename,".html")
    b<-paste0('<li><h2 class="vignettes"><a href="',link,'">',p$title,'</a></h2></li>\n')
    a<-paste(a,b)
  }
  a<-paste(ul,a,'</ul>\n')

  return(a)
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



#' print a list of options and description from yaml file
#' @param com the command to look for, as in com.a.yaml
#' @export

format_options<-function(com,optnames) {

  output<-"<table class='options'>"
  opts<-get_options(com,optnames)
  .format_options(opts)

}

.format_options<-function(opts) {

  output<-"<table class='options'>"
  for (opt in opts) {
    if (length(opt)==1) opt<-opt[[1]]

    desc<-"No description"
    if (hasName(opt$description,"R"))
      desc<-opt$description$R
    if (hasName(opt$description,"ui"))
      desc<-opt$description$ui
    if (!is.list(opt$description))
      desc<-opt$description
    output<-paste(output,"<tr>")
    title<-try(ifelse(hasName(opt,"label"),opt$label,opt$title))
    if ("try-error" %in% class(title)) {
      print(opt)
      stop()
    }
    output<-paste(output,paste("<td class='optionname'> ",opt(title),"</td> <td class='optionvalue'>",desc,"</td>\n"))
    output<-paste(output,"</tr>")

  }
  output<-paste(output,"</table>")

  return(output)
}



#' print a list of options within a panel
#' @param com the name of the panel to look for, as in .u.yaml
#' @export
### we need this because I need to be sure that labeling do not fuck up with old docs
### remove as soon as possible

panel_options<-function(com,value, labels=FALSE, mode=NULL) {

   if (labels) .panel_options_new(com,value,mode)
   else .panel_options_old(com,value,mode)

}

.panel_options_new<-function(com,value, mode=NULL) {

  obj<-get_u(com)
  panel<-find_in_list(obj,"name",value)
  if (length(panel)==0)
      return()
  panel<-panel[[1]]

  if (!is.null(mode)) {
    panel<-filter_list(panel,"Content",mode)
  }

  opts<-find_in_list(panel,"type",c("CheckBox","RadioButton","ComboBox","Output","TextBox"))
  opts<-unique(unlist(lapply(opts,function(x) if (hasName(x,"optionName")) x$optionName else x$name)))
  results<-list()
  labs<-find_in_list(panel,"type",c("Label"))
  for (lab in labs) {
        lopts<-find_in_list(lab,"type",c("CheckBox","RadioButton","ComboBox","Output","TextBox"))
        if (length(lopts)==length(find_in_list(lab,"type","RadioButton"))) {
          next
        }
        lopts<-unique(unlist(lapply(lopts,function(x) if (hasName(x,"optionName")) x$optionName else x$name)))
        opts<-opts[!(opts %in% lopts)]
        results[[length(results)+1]]<-list(title=lab$label,children=lopts)
  }

  text<-""

  if (length(opts)>0)   text<-format_options(com,opts)
  for (result in results) {
    text<-paste(text,paste("\n<h4 >",opt_label(result$title),"</h4>\n"),"\n")
    text<-paste(text,format_options(com,result$children))
  }
  return(text)
}

.panel_options_old<-function(com,value,mode=NULL) {
  obj<-get_u(com)
  panel<-find_in_list(obj,"name",value)
  if (length(panel)==0)
      return()
  panel<-panel[[1]]
  opts<-find_in_list(panel,"type",c("CheckBox","RadioButton","ComboBox","Output","TextBox"))
  opts<-unique(unlist(lapply(opts,function(x) if (hasName(x,"optionName")) x$optionName else x$name)))
  format_options(com,opts)
}

#' print output variable descriptions
#' @export

format_outputs<-function(com) {

  file<-paste0(MODULE_FOLDER,"/jamovi/",com,".a.yaml")
  obj<<-yaml::read_yaml(file)
  options<-obj$options
  res<-rlist::list.find(options,type=="Output",n=Inf)
  .format_options(res)
}



find_in_list<-function(obj,what,value) {

  res<-list()
.recurse<-function(obj) {

  if (hasName(obj,what))
    if (obj[[what]] %in% value) {
      res[[length(res)+1]]<<-obj
   }

 if (hasName(obj,"children"))
    for (child in obj$children)
              .recurse(child)

}
.recurse(obj)
res
}

filter_list <- function(lst, type_value, name_value) {
  if (!is.list(lst)) return(lst)

  # check if this node itself has a type field
  if (!is.null(lst[["type"]]) && identical(lst[["type"]], type_value)) {
    # if type matches but name mismatches -> remove it
    if (is.null(lst[["name"]]) || !identical(lst[["name"]], name_value)) {
      return(NULL)
    }
  }

  # otherwise recurse into children
  children <- lapply(lst, filter_list,
                     type_value = type_value, name_value = name_value)

  # remove NULL children
  keep <- !vapply(children, is.null, logical(1))
  children[keep]
}


get_u<-function(com) {

    file<-paste0(MODULE_FOLDER,"/jamovi/",com,".u.yaml")
    obj<<-yaml::read_yaml(file)
    obj

  }
