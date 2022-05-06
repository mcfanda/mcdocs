#' Create a new project
#'
#' create a new project in the a folder
#' @param folder the target folder
#' @author Marcello Gallucci
#' @rdname createdocs
#' @export
createdocs <- function(folder="./") {

   test<-dir.exists(folder)
   if (test)
      cat("Using existing folder ",folder)
   else {
      cat("creating folder ",folder)
      dir.create(folder)
   }
   cat("\n\n")
   if (!dir.exists(paste0(folder,SOURCE_FOLDER)))
          dir.create(paste0(folder,SOURCE_FOLDER))
   if (!dir.exists(paste0(folder,SOURCE_FOLDER,"/pics")))
          dir.create(paste0(folder,SOURCE_FOLDER,"/pics"))
   if (!dir.exists(paste0(folder,"/R")))
          dir.create(paste0(folder,"/R"))

      if (!file.exists(paste0(folder,SOURCE_FOLDER,"/bib.yaml")))
       file.create(paste0(folder,SOURCE_FOLDER,"/bib.yaml"))

   if (!file.exists(paste0(folder,SOURCE_FOLDER,"jamovi.css")))
        file.copy(system.file("jamovi.css", package="mcdocs"),paste0(folder,SOURCE_FOLDER))
   if (!file.exists(paste0(folder,SOURCE_FOLDER,"/_site.yml")))
        file.copy(system.file("_site.yml", package="mcdocs"),paste0(folder,SOURCE_FOLDER))
   if (!file.exists(paste0(folder,SOURCE_FOLDER,"/ganalytics.txt")))
       file.create(paste0(folder,SOURCE_FOLDER,"/ganalytics.txt"))

   if (!file.exists(paste0(folder,"/R/info.R")))
     file.copy(system.file("info.R", package="mcdocs"),paste0(folder,"/R"))

   if (!file.exists(paste0(folder,"/R/newversion.R")))
      file.copy(system.file("newversion.R", package="mcdocs"),paste0(folder,"/R"))

}

#' render the site
#' @export
render_mcdocs<-function() {

   copy<-FALSE

   if (SOURCE_FOLDER==STORE_FOLDER)
       stop("Source folder cannot be the same as the store folder")
   here<-getwd()

   if (!dir.exists(SOURCE_FOLDER))
      stop("Folder ",SOURCE_FOLDER," does not exist. Rendering aborted.")
   if (!dir.exists(STORE_FOLDER))
      stop("Folder",SOURCE_FOLDER," does not exist. Rendering aborted.")

   if (!is.null(TARGET_FOLDER)) {
        if (SOURCE_FOLDER!=TARGET_FOLDER) {
           if (!dir.exists(TARGET_FOLDER))
              stop("Folder",TARGET_FOLDER," does not exist. Rendering aborted.")
           else
              copy=TRUE
        }
   }
    rmarkdown::render_site(SOURCE_FOLDER,output_format = "html_document")

    if (copy) {
       cmd<-paste("cp -R ",paste0(STORE_FOLDER,"/*"),paste0(TARGET_FOLDER,"/"))
       system(cmd)
    }

}
#' update git repository
#' @export
update_git<-function(){

   msg<-"updates"
   system("git add .")
   system(paste('git commit -m "',msg,'"'))
   system("git push ")


}

#' write commits after cleaning
#' @export
write_commits<-function(commits) {

   current<-get_github_current()
   sel<-list()
   j<-1
   for (i in 1:dim(commits)[1]) {
      msg<-trimws(commits[i,"msg"])
      gonext=FALSE
      try({
         if (!is.null(BANNED_COMMITS))
            for (rule in BANNED_COMMITS) {
               if (msg==rule)
                  gonext=TRUE
            }
      })
      for (rule in BANNED_COMMITS_GREP) {
         if (length(grep(rule,msg)))
            gonext=TRUE
      }

      if (gonext)
         next()
      test<-grep("§",msg,fixed=T)
      if (length(test)>0) msg<-paste("<b>",msg,"</b>")
      sel[[j]]<-c(msg=msg,version=commits[i,"version"])
      j<-j+1
   }
   sel<-rev(sel)
   coms<-as.data.frame(do.call("rbind",sel))
   versions<-unlist(rev(unique(coms$version)))
   cv<-ifelse(!is.null(current),current,".x.x.x.")


   for (i in seq_along(versions)) {
      rel<-""
      if (versions[i]==cv)
         rel<-"(Current)"
      cat(paste("#",versions[i],rel,"\n\n"))
      cs<-coms[coms[,2]==versions[i],1]
      for (j in cs)
         cat(paste("*",j,"\n\n"))
   }
   #coms
}


#' get current version
#' @export
get_github_current<-function() {


   query<-"/repos/:owner/:repo/pulls?state=closed"
   pulls<-gh::gh(query,
                 owner = MODULE_REPO_OWNER,
                 repo = MODULE_REPO,
                 .limit=Inf,
                 .token=API_TOKEN)
   cv<-pulls[[1]]
   cv$head$ref
}

#' get versions
#' @export
get_github_pulls<-function() {

   query<-"/repos/:owner/:repo/pulls?state=closed"
   pulls<-gh::gh(query,
                 owner = MODULE_REPO_OWNER,
                 repo = MODULE_REPO,
                 .limit=Inf,
                 .token=API_TOKEN)
   rlist::list.clean(pulls,function(p) p$head$ref=="develop")
}



#' get all commits
#' @export
get_github_commits<-function(pulls) {


   commits<-list()

   for (pull in pulls) {
      cu<-pull$commits_url
      query<-gsub("https://api.github.com","",cu,fixed = T)

      coms<-gh::gh(query,
                   .limit=Inf,
                   .token=API_TOKEN)
      for (com in coms) {
         if (length(com)==0)
            next
         one<-list(sha=com$sha,msg=com$commit$message,version=pull$head$ref)
         commits[[length(commits)+1]]<-one
      }
   }
   commits<-as.data.frame(do.call(rbind,commits))
   commits
}




#' save new commits in old commits file
#' @export
get_commits<-function() {

   folder<-paste0(PROJECT_FOLDER,"/resources")
   if (!dir.exists(folder))
        dir.create(folder)

   cfile<-paste0(folder,"/commitsdata.Rda")
   present_vers<-list()
   commits<-data.frame()
   test<-file.exists(cfile)
   if (test) {
      load(cfile)
      present_vers<-unlist(unique(commits$version))
   }

   all_pulls<-get_github_pulls()
   all_vers<-unlist(rlist::list.select(all_pulls, head$ref ))
   sel<-!(all_vers %in% present_vers)
   pulls<-all_pulls[sel]
   if (length(pulls)==0) {
        return(commits)
   }

   test<-unlist(rlist::list.select(pulls, head$ref ))
   newcommits<-get_github_commits(pulls)
   commits<-rbind(newcommits,commits)
   save(commits, file=cfile)
   commits
}

#' load info file from project R folder
#' @export
mcdocs_init<-function() {

   rwhere<-"R/"
   if (!dir.exists(rwhere))
      rwhere<-"../R/"
   if (!dir.exists(rwhere))
      stop("R/ folder not found")

   if (!file.exists(paste0(rwhere,"info.R")))
      stop("File R/info.R not found")

   source(paste0(rwhere,"info.R"))

   invisible(rwhere)

}

#' load info file for github secrets
#' @export
mcdocs_gh_info<-function() {
   rwhere<-"R/"
   if (!dir.exists(rwhere))
      rwhere<-"../R/"
   if (!dir.exists(rwhere))
      stop("R/ folder not found")

    if (!file.exists(paste0(rwhere,"secrets.R")))
        stop("File R/secrets.R not found. Create it to set github `API_TOKEN` ")

    source(paste0(rwhere,"secrets.R"))
}
