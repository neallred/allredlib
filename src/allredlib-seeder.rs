use anyhow::Result;
use serde::{Serialize, Deserialize};
use std::collections::HashMap;

mod attribution;
mod creator;
mod res;
mod series;
mod subseries;
mod subseries_attribution;
mod title;
mod genre;
mod title_genre;

use attribution::{Attribution, AttributionRequest};
use creator::{Creator, CreatorRequest};
use series::{Series, SeriesRequest};
use subseries::{Subseries, SubseriesRequest};
use subseries_attribution::{SubseriesAttribution, SubseriesAttributionRequest};
use title::{Title, TitleRequest};
use genre::{Genre, GenreRequest};
use title_genre::{TitleGenre, TitleGenreRequest};

static B_CREATORS: &'static [u8] = include_bytes!("../seed/Creator.json");
static B_SERIES: &'static [u8] = include_bytes!("../seed/Series.json");
static B_SUBSERIES: &'static [u8] = include_bytes!("../seed/Subseries.json");
static B_ATTRIBUTIONS: &'static [u8] = include_bytes!("../seed/Attribution.json");
static B_TITLES: &'static [u8] = include_bytes!("../seed/Title.json");
static B_GENRES: &'static [u8] = include_bytes!("../seed/Genre.json");
static B_TITLE_GENRES: &'static [u8] = include_bytes!("../seed/TitleGenre.json");

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
#[serde(rename_all = "camelCase")]
struct SubseriesSeed {
    id: String,
    synopsis: Option<String>,
    title: String,
    total_book_members: i32,
    series_id: String,
    attribution_ids: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct TitleSeed {
    id: String,
    title: String,
    year: Option<i32>,
    synopsis: Option<String>,
    series_part: Option<String>,
    series_id: Option<String>,
    subseries_part: Option<String>,
    subseries_id: Option<String>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct TitleGenreSeed {
    title_id: String,
    genre_id: String,
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
    let subseries_ids = seed_subseries(&client, &host, B_SUBSERIES, &attribution_ids, &series_ids).await?;
    let title_ids = seed_titles(&client, &host, B_TITLES, &series_ids, &subseries_ids).await?;
    let genre_ids = seed_genres(&client, &host, B_GENRES, &title_ids).await?;
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
        for x in &seed {
            let id = seed_ids[counter - 1].id.clone();
            print!("\rInserting {} {} of {}", path, counter, count);
            let added = client.post(&url)
                .json(&x)
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

async fn seed_genres(client: &reqwest::Client, host: &String, bytes: &[u8], title_ids: &IdMap) -> Result<IdMap> {
    let mut idmap: IdMap = HashMap::new();
    let url: String = format!("{}/genres", host);
    let title_genres_url: String = format!("{}/title_genres", host);
    let has_resource = reqwest::get(&url)
        .await?
        .json::<Vec<Genre>>()
        .await?.len() > 0;

    if has_resource {
        return Ok(idmap)
    }

    let genres: Vec<String> = serde_json::from_slice(bytes)?;
    print!("\n");
    let count = genres.len();
    let mut counter: usize = 1;
    for x in &genres {
        print!("\rInserting genres {} of {}", counter, count);

        let genre_request = GenreRequest {
            genre: x.clone(),
        };
        let id = counter;
        let added = client.post(&url)
            .json(&genre_request)
            .send()
            .await?
            .json::<Genre>()
            .await?;
        let db_id: DbId = serde_json::from_str(&serde_json::to_string(&added)?.to_string())?;
        idmap.insert(id.to_string(), db_id.id);

        counter = counter + 1;
    }
    print!("\n");

    let title_genre_requests: Vec<TitleGenreSeed> = serde_json::from_slice(B_TITLE_GENRES)?;

    print!("\n");
    let mut counter: usize = 1;
    let title_genre_requests_len = title_genre_requests.len();
    for title_genre in title_genre_requests {
        print!("\rInserting title genres {} of {}", counter, title_genre_requests_len);
        let added = client.post(&title_genres_url)
            .json(&TitleGenreRequest {
                title_id: title_genre.title_id.parse().unwrap(),
                genre_id: title_genre.genre_id.parse().unwrap(),
            })
            .send()
            .await?
            .json::<TitleGenre>()
            .await?;
        counter = counter + 1;
    }
    print!("\n");

    Ok(idmap)
}

async fn seed_subseries(client: &reqwest::Client, host: &String, bytes: &[u8], attribution_ids: &IdMap, series_ids: &IdMap) -> Result<IdMap> {
    let mut idmap: IdMap = HashMap::new();
    let url: String = format!("{}/subseries", host);
    let attribution_url: String = format!("{}/subseries_attributions", host);
    let has_resource = reqwest::get(&url)
        .await?
        .json::<Vec<Subseries>>()
        .await?.len() > 0;

    if has_resource {
        return Ok(idmap)
    }

    let seed: Vec<SubseriesSeed> = serde_json::from_slice(bytes)?;
    print!("\n");
    let count = seed.len();
    let mut counter: usize = 1;
    let seed_ids: Vec<SeedId> = serde_json::from_slice(bytes)?;
    for x in &seed {
        print!("\rInserting subseries {} of {}", counter, count);

        let subseries_request = SubseriesRequest {
            synopsis: x.synopsis.clone(),
            title: x.title.clone(),
            total_book_members: x.total_book_members,
            series_id: *series_ids.get(&x.series_id).unwrap(),
        };
        let id = seed_ids[counter - 1].id.clone();
        let added = client.post(&url)
            .json(&subseries_request)
            .send()
            .await?
            .json::<Subseries>()
            .await?;
        let db_id: DbId = serde_json::from_str(&serde_json::to_string(&added)?.to_string())?;
        idmap.insert(id, db_id.id);

        for attribution_id in &x.attribution_ids  {
            let added = client.post(&attribution_url)
                .json(&SubseriesAttributionRequest{
                    subseries_id: db_id.id,
                    attribution_id: *attribution_ids.get(attribution_id).unwrap(),
                })
                .send()
                .await?
                .json::<SubseriesAttribution>()
                .await?;

        }

        counter = counter + 1;
    }
    print!("\n");
    Ok(idmap)
}

fn some_to_value<T: Copy>(src: Option<&T>) -> Option<T> {
    match src {
        Some(x) => Some(*x),
        None => None
    }
}

async fn seed_titles(client: &reqwest::Client, host: &String, bytes: &[u8], series_ids: &IdMap, subseries_ids: &IdMap) -> Result<IdMap> {
    let mut idmap: IdMap = HashMap::new();
    let url: String = format!("{}/titles", host);
    let has_resource = reqwest::get(&url)
        .await?
        .json::<Vec<Title>>()
        .await?.len() > 0;

    if has_resource {
        return Ok(idmap)
    }

    let seed: Vec<TitleSeed> = serde_json::from_slice(bytes)?;
    print!("\n");
    let count = seed.len();
    let mut counter: usize = 1;
    let seed_ids: Vec<SeedId> = serde_json::from_slice(bytes)?;
    for x in &seed {
        print!("\rInserting title {} of {}", counter, count);

        let title_request = TitleRequest {
            title: x.title.clone(),
            year: x.year,
            synopsis: x.synopsis.clone(),
            series_part: x.series_part.clone(),
            series_id: some_to_value(series_ids.get(&x.series_id.clone().unwrap_or(String::from("")))),
            subseries_part: x.subseries_part.clone(),
            subseries_id: some_to_value(subseries_ids.get(&x.subseries_id.clone().unwrap_or(String::from("")))),
        };
        let id = seed_ids[counter - 1].id.clone();
        let added = client.post(&url)
            .json(&title_request)
            .send()
            .await?
            .json::<Title>()
            .await?;
        let db_id: DbId = serde_json::from_str(&serde_json::to_string(&added)?.to_string())?;
        idmap.insert(id, db_id.id);

        counter = counter + 1;
    }
    print!("\n");
    Ok(idmap)
}
