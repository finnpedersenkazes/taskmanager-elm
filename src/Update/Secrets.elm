module Update.Secrets exposing (appId)


appId : String
appId =
    "113439d46d7536fcab42240ae383a52f"



{-
   Example
   https://api.openweathermap.org/data/2.5/weather?q=Virum,dk&APPID=113439d46d7536fcab42240ae383a52f&units=metric

-}
{-
      Database
        create_table "tasks", force: :cascade do |t|
          t.string "title"
          t.text "description"
          t.integer "urgency"
          t.integer "duration_minutes"
          t.date "attention_date"
          t.date "deadline"
          t.date "planned_date"
          t.time "planned_starting_time"
          t.integer "status"
          t.datetime "created_at", null: false
          t.datetime "updated_at", null: false
        end


   {"id":26
     "title":"Scabbers"
     "description":"You can't transmit the transmitter without quantifying the open-source COM card!"
     "urgency":"dont_do_it"
     "duration_minutes":90
     "attention_date":"2018-01-17"
     "deadline":"2018-01-22"
     "planned_date":"2018-01-19"
     "planned_starting_time":"2000-01-01T19:30:00.000Z"
     "status":"planned"
     "created_at":"2018-01-17T08:54:53.625Z"
     "updated_at":"2018-01-17T08:54:53.625Z"
    }

-}
