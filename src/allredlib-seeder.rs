use anyhow::Result;

mod creator;

static CREATORS_BYTES: &'static [u8] = include_bytes!("../seed/Creator.json");

// TODO:
// support passing address as string, env var, arg, default

#[tokio::main]
async fn main() -> Result<()> {
    let client = reqwest::Client::new();
    seed_creators(&client).await?;
    Ok(())
}

async fn seed_creators(client: &reqwest::Client) -> Result<()> {
    let creator_requests: Vec<creator::CreatorRequest> = serde_json::from_slice(CREATORS_BYTES)?;

    let has_creators = reqwest::get("http://localhost:5000/creators")
        .await?
        .json::<Vec<creator::Creator>>()
        .await?.len() > 0;

    if !has_creators {
        for creator in &creator_requests {
            client.post("http://localhost:5000/creator")
                .json(&creator)
                .send()
                .await?
                .json::<creator::Creator>()
                .await?;
        }
        println!("Inserted {} creators", creator_requests.len());
    }
    Ok(())
}
