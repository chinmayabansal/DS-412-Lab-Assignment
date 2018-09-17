flag=1
i=1
check =10    
while(i<11)
{  
  if (check>(input$plot_brush$xmax+10))
  {
    flag=2
  }
  if((input$plot_brush$xmin<check) && (flag==1))
  {
    color[[i]]<-"orange"
  }  
  else{
    color[[i]]<-"blue"
  }
  i=i+1
  check=check+10
  print(i)
}


