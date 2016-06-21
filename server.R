library(shiny)
library(datasets)

# Define server logic required to summarize and view the selected
# dataset
shinyServer(function(input, output) {
  
  output$text1 <- renderText({ 
    paste("You have selected", input$radio)
  })
 output$keywords<-renderText({ 
   paste("Keywords:")
 })
 output$documents<-renderText({ 
   paste("Documents:")
 })
  
  output$wordcloud <- renderPlot({
    
    url<-'http://localhost:8983/solr/gettingstarted/select'
    url
    query=input$query
    inputdf<-solr_search(q= query,fl='id',base = url)
    inputdf
    output$text2<-renderText({ 
           paste(inputdf[1:nrow(inputdf),],"",sep = "\n")
    })
    Rpdf <- readPDF(control = list(text = "-layout"))
    texts<-Corpus(URISource(c(inputdf[1: nrow(inputdf),]),mode = "text"), readerControl = list(reader = Rpdf))
    texts<-tm_map(texts,removeNumbers)
    for(j in seq(texts))   
    {   
      texts[[j]] <- gsub("-", " ", texts[[j]])   
    }   
    texts<-tm_map(texts,removePunctuation)
    texts<-tm_map(texts,tolower)
    texts<-tm_map(texts,stemDocument)  
    texts<-tm_map(texts,removeWords,stopwords("english"))  
    texts<-tm_map(texts,stripWhitespace) 
    texts <- tm_map(texts, PlainTextDocument)
    
    texts.tdm<-TermDocumentMatrix(texts) 
    texts.tfidf<-TermDocumentMatrix(texts, control=list(weighting = function(x) weightTfIdf(x,normalize = T)) ) 
      wt<-inspect(texts.tdm)
      w<-inspect(texts.tfidf)
      
      output$text3<-renderText({ 
                                
                               if(nrow(inputdf)==1){
                               paste(rownames(as.data.frame(sort(rowSums(wt),decreasing = T)))[1:4],sep = "\n")}
                               else{
                                 paste(rownames(as.data.frame(sort(rowSums(w),decreasing = T)))[1:4],sep = "\n")}
                               
                                    })
  if(input$radio=="normal"){  
    if(nrow(inputdf)==1){ wordcloud(row.names(wt),rowSums(wt),max.words = 60,color=brewer.pal(8,"Dark2"))}
  else { wordcloud(row.names(w),rowSums(w),max.words = 60,color=brewer.pal(8,"Dark2")) }
  }
  else if(input$radio=="associative"){
    cs<-(w%*%t(w))
    c<-row.names(cs)
    cs[lower.tri(cs,diag = TRUE)]<-0
    l<-length(cs[1,])
    i=1
    w1<-0
    w2<-0
    f<-0
    while(max(cs)!=0&&i<=100)
    {
      m<-which(max(cs)==cs)
      m<-m[1]
      cat(m)
      w1[i]<-(c[m%%l])#matching column
      w2[i]<-(c[(m%/%l)+1])#matching row
      f[i]<-cs[m]
      cs[m]<-0
      i<-i+1
    }
    w<-paste(w1,w2)
    w[1:100]
    f[1:100]
    wordcloud(w[1:35], f[1:35],max.words = 35,color=brewer.pal(8,"Dark2"))
    
  }
  })
})