#"automated email.R" script

library(emayili)

warning_email <- envelope(
  to = "cboewig@gmail.com",
  from = "ida.eec@gmail.com",
  subject = "WARNING: Suspekte fly over Nordsøen⚠️",
  text = "Der er observeret suspekte fly henover Nordsøen. Her er en liste med dem:\n\n"
) |>
  attachment("suspekte_fly.csv")


smtp <- server(
  host = "smtp.gmail.com",
  port = 465,
  username = Sys.getenv("GMAIL_USERNAME"),
  password = Sys.getenv("GMAIL_PASSWORD")
)