use tbot::{bot, prelude::*, Bot};

fn build_bot() -> Bot {
    let mut builder = bot::Builder::with_env_token("BOT_TOKEN");

    if let Ok(uri) = std::env::var("BOT_API_URI") {
        builder = builder.server_uri(uri.parse().unwrap());
    }

    builder.build()
}

#[tokio::main]
async fn main() {
    let mut bot = build_bot().event_loop();

    bot.start(|context| async move {
        context
            .send_message("Hello! Send me a message and I'll reverse it.")
            .call()
            .await
            .unwrap();
    });

    bot.text(|context| async move {
        let reversed: String = context.text.value.chars().rev().collect();

        context.send_message(reversed).call().await.unwrap();
    });

    bot.polling().start().await.unwrap();
}
