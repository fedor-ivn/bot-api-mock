use tbot::{bot, Bot};

fn build_bot() -> Bot {
    let mut builder = bot::Builder::with_env_token("BOT_TOKEN");

    if let Ok(uri) = std::env::var("BOT_API_URI") {
        builder = builder.server_uri(uri.parse().unwrap());
    }

    builder.build()
}

#[tokio::main]
async fn main() {
    let bot = build_bot();

    let response = bot.get_me().call().await;
    println!("{response:#?}");
}
