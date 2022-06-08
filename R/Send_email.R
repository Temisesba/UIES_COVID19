library(mailR)
msg <- paste("\n\n" , "Adjunto el documento en Google Drive mediante el siguiente enlace: ", "\n\n"  )

sender <- "alanzambie@gmail.com"
recipients <- c("alanzambie@gmail.com" )
carb_copy <- "alanzambie@gmail.com"

send.mail(from = sender,
          to = recipients,
          cc = carb_copy,
          subject = paste("Base COVID-19 Corte", format(Sys.Date(), "%d.%m.%Y")),
          body = iconv(msg, to = "UTF-8"),

          smtp = list(host.name = "smtp.gmail.com", port = 587,
                      user.name = "correo@gmail.com",
                      passwd = "password", ssl = TRUE),

          attach.files = c("Constancias/constancias_sanidad-10.pdf"),

          authenticate = TRUE,
          send = TRUE)

