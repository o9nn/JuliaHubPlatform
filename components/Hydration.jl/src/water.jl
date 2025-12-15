
const WATER_REMINDER_MESSAGES = (
    "How long since you last drank water?",
    "Don't forget to drink water!",
    "Water is important for life!"
)

function print_water_reminder(io)
    print(io, "🥤 ")
    msg = rand(WATER_REMINDER_MESSAGES)
    printstyled(io, msg; color=:blue, bold=true)
    println(io)
end

const LAST_REMEMBER_WATER = Ref(DateTime(0))
remember_water(io, period) = is_it_time!(LAST_REMEMBER_WATER, period) && print_water_reminder(io)
