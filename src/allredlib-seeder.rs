use anyhow::Result;
use serde::{Serialize, Deserialize};
use std::collections::HashMap;

mod creator;
use creator::{Creator, CreatorRequest};
mod series;
use series::{Series, SeriesRequest};
mod attribution;
use attribution::{Attribution, AttributionRequest};

static B_CREATORS: &'static [u8] = include_bytes!("../seed/Creator.json");
static B_SERIES: &'static [u8] = include_bytes!("../seed/Series.json");
static B_SUBSERIES: &'static [u8] = include_bytes!("../seed/Series.json");
static B_ATTRIBUTIONS: &'static [u8] = include_bytes!("../seed/Attribution.json");

type IdMap = HashMap<String, i64>;

#[derive(Serialize, Deserialize, Debug)]
struct SeedId {
    id: String
}
#[derive(Serialize, Deserialize, Debug)]
struct DbId {
    id: i64
}
#[derive(Serialize, Deserialize, Debug)]
struct SubseriesIds {
    id: String,
    series_id: String,
    attribution_ids: Vec<String>,
}
#[derive(Serialize, Deserialize, Debug)]
struct SubseriesSeed {
    id: String,
    synopsis: Option<String>,
    title: String,
    total_book_members: i32,
    series_id: String,
    attribution_ids: Vec<String>,
}

// TODO:
// support passing address as string, env var, arg, default

#[tokio::main]
async fn main() -> Result<()> {
    let client = reqwest::Client::new();
    let host = String::from("http://localhost:5000");
    let creator_ids = seed::<CreatorRequest, Creator>(&client, &host, "creators", B_CREATORS).await?;
    let series_ids = seed::<SeriesRequest, Series>(&client, &host, "series", B_SERIES).await?;
    let attribution_ids = seed::<AttributionRequest, Attribution>(&client, &host, "attributions", B_ATTRIBUTIONS).await?;
    let subseries_ids = seed_subseries(&client, &host, B_SUBSERIES, &attribution_ids).await?;
    Ok(())
}

async fn seed<T: serde::de::DeserializeOwned + serde::ser::Serialize, U: serde::de::DeserializeOwned + serde::ser::Serialize>(client: &reqwest::Client, host: &String, path: &str, bytes: &[u8]) -> Result<IdMap> {
    let mut idmap: IdMap = HashMap::new();
    let seed: Vec<T> = serde_json::from_slice(bytes)?;
    let seed_ids: Vec<SeedId> = serde_json::from_slice(bytes)?;
    let url: String = format!("{}/{}", host, path);
    let has_resource = reqwest::get(&url)
        .await?
        .json::<Vec<U>>()
        .await?.len() > 0;

    if !has_resource {
        print!("\n");
        let count = seed.len();
        let mut counter: usize = 1;
        for creator in &seed {
            let id = seed_ids[counter - 1].id.clone();
            print!("\rInserting {} {} of {}", path, counter, count);
            let added = client.post(&url)
                .json(&creator)
                .send()
                .await?
                .json::<U>()
                .await?;
            let db_id: DbId = serde_json::from_str(&serde_json::to_string(&added)?.to_string())?;
            idmap.insert(id, db_id.id);
            counter = counter + 1;
        }
        print!("\n");
    }
    Ok(idmap)
}

// async fn seed_subseries(client: &reqwest::Client, host: &String, bytes: &[u8], attribution_ids: &IdMap) -> Result<IdMap> {
//     let mut idmap: IdMap = HashMap::new();
//     let url: String = format!("{}/subseries", host);
//     let has_resource = reqwest::get(&url)
//         .await?
//         .json::<Vec<U>>()
//         .await?.len() > 0;
// 
//     if has_resource {
//         return Ok(idmap())
//     }
// 
//     let seed: Vec<SubseriesSeed> = serde_json::from_slice(bytes)?;
//     print!("\n");
//     let count = seed.len();
//     let mut counter: usize = 1;
//     for creator in &seed {
//         let id = seed_ids[counter - 1].id.clone();
//         print!("\rInserting {} {} of {}", path, counter, count);
//         let added = client.post(&url)
//             .json(&creator)
//             .send()
//             .await?
//             .json::<U>()
//             .await?;
//         let db_id: DbId = serde_json::from_str(&serde_json::to_string(&added)?.to_string())?;
//         idmap.insert(id, db_id.id);
//         counter = counter + 1;
//     }
//     print!("\n");
//     Ok(idmap)
// }
