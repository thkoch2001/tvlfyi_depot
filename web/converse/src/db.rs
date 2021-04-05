// Copyright (C) 2018-2021 Vincent Ambo <tazjin@tvl.su>
//
// This file is part of Converse.
//
// This program is free software: you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see
// <https://www.gnu.org/licenses/>.

//! This module implements the database executor, which holds the
//! database connection and performs queries on it.

use actix::prelude::*;
use diesel::{self, sql_query};
use diesel::sql_types::Text;
use diesel::prelude::*;
use diesel::r2d2::{Pool, ConnectionManager};
use crate::models::*;
use crate::errors::{ConverseError, Result};

/// Raw PostgreSQL query used to perform full-text search on posts
/// with a supplied phrase. For now, the query language is hardcoded
/// to English and only "plain" queries (i.e. no searches for exact
/// matches or more advanced query syntax) are supported.
const SEARCH_QUERY: &'static str = r#"
WITH search_query (query) AS (VALUES (plainto_tsquery('english', $1)))
SELECT post_id,
       thread_id,
       author,
       title,
       ts_headline('english', body, query) AS headline
  FROM search_index, search_query
  WHERE document @@ query
  ORDER BY ts_rank(document, query) DESC
  LIMIT 50
"#;

const REFRESH_QUERY: &'static str = "REFRESH MATERIALIZED VIEW search_index";

pub struct DbExecutor(pub Pool<ConnectionManager<PgConnection>>);

impl DbExecutor {
    /// Request a list of threads.
    //
    // TODO(tazjin): This should support pagination.
    pub fn list_threads(&self) -> Result<Vec<ThreadIndex>> {
        use crate::schema::thread_index::dsl::*;

        let conn = self.0.get()?;
        let results = thread_index
            .load::<ThreadIndex>(&conn)?;
        Ok(results)
    }

    /// Look up a user based on their email-address. If the user does
    /// not exist, it is created.
    pub fn lookup_or_create_user(&self, user_email: &str, user_name: &str) -> Result<User> {
        use crate::schema::users;
        use crate::schema::users::dsl::*;

        let conn = self.0.get()?;

        let opt_user = users
            .filter(email.eq(email))
            .first(&conn).optional()?;

        if let Some(user) = opt_user {
            Ok(user)
        } else {
            let new_user = NewUser {
                email: user_email.to_string(),
                name: user_name.to_string(),
            };

            let user: User = diesel::insert_into(users::table)
                .values(&new_user)
                .get_result(&conn)?;

            info!("Created new user {} with ID {}", new_user.email, user.id);

            Ok(user)
        }
    }

    /// Fetch a specific thread and return it with its posts.
    pub fn get_thread(&self, thread_id: i32) -> Result<(Thread, Vec<SimplePost>)> {
        use crate::schema::threads::dsl::*;
        use crate::schema::simple_posts::dsl::id;

        let conn = self.0.get()?;
        let thread_result: Thread = threads
            .find(thread_id).first(&conn)?;

        let post_list = SimplePost::belonging_to(&thread_result)
            .order_by(id.asc())
            .load::<SimplePost>(&conn)?;

        Ok((thread_result, post_list))
    }

    /// Fetch a specific post.
    pub fn get_post(&self, post_id: i32) -> Result<SimplePost> {
        use crate::schema::simple_posts::dsl::*;
        let conn = self.0.get()?;
        Ok(simple_posts.find(post_id).first(&conn)?)
    }

    /// Update the content of a post.
    pub fn update_post(&self, post_id: i32, post_text: String) -> Result<Post> {
        use crate::schema::posts::dsl::*;
        let conn = self.0.get()?;
        let updated = diesel::update(posts.find(post_id))
            .set(body.eq(post_text))
            .get_result(&conn)?;

        Ok(updated)
    }

    /// Create a new thread.
    pub fn create_thread(&self, new_thread: NewThread, post_text: String) -> Result<Thread> {
                use crate::schema::threads;
        use crate::schema::posts;

        let conn = self.0.get()?;

        conn.transaction::<Thread, ConverseError, _>(|| {
            // First insert the thread structure itself
            let thread: Thread = diesel::insert_into(threads::table)
                .values(&new_thread)
                .get_result(&conn)?;

            // ... then create the first post in the thread.
            let new_post = NewPost {
                thread_id: thread.id,
                body: post_text,
                user_id: new_thread.user_id,
            };

            diesel::insert_into(posts::table)
                .values(&new_post)
                .execute(&conn)?;

            Ok(thread)
        })
    }

    /// Create a new post.
    pub fn create_post(&self, new_post: NewPost) -> Result<Post> {
        use crate::schema::posts;

        let conn = self.0.get()?;

        let closed: bool = {
            use crate::schema::threads::dsl::*;
            threads.select(closed)
                .find(new_post.thread_id)
                .first(&conn)?
        };

        if closed {
            return Err(ConverseError::ThreadClosed {
                id: new_post.thread_id
            })
        }

        Ok(diesel::insert_into(posts::table)
           .values(&new_post)
           .get_result(&conn)?)
    }

    /// Search for posts.
    pub fn search_posts(&self, query: String) -> Result<Vec<SearchResult>> {
        let conn = self.0.get()?;

        let search_results = sql_query(SEARCH_QUERY)
            .bind::<Text, _>(query)
            .get_results::<SearchResult>(&conn)?;

        Ok(search_results)
    }

    /// Trigger a refresh of the view used for full-text searching.
    pub fn refresh_search_view(&self) -> Result<()> {
        let conn = self.0.get()?;
        debug!("Refreshing search_index view in DB");
        sql_query(REFRESH_QUERY).execute(&conn)?;
        Ok(())
    }
}


// Old actor implementation:

impl Actor for DbExecutor {
    type Context = SyncContext<Self>;
}

/// Message used to look up a user based on their email-address. If
/// the user does not exist, it is created.
pub struct LookupOrCreateUser {
    pub email: String,
    pub name: String,
}

message!(LookupOrCreateUser, Result<User>);

impl Handler<LookupOrCreateUser> for DbExecutor {
    type Result = <LookupOrCreateUser as Message>::Result;

    fn handle(&mut self,
              _: LookupOrCreateUser,
              _: &mut Self::Context) -> Self::Result {
        unimplemented!()
    }
}

/// Message used to fetch a specific thread. Returns the thread and
/// its posts.
pub struct GetThread(pub i32);
message!(GetThread, Result<(Thread, Vec<SimplePost>)>);

impl Handler<GetThread> for DbExecutor {
    type Result = <GetThread as Message>::Result;

    fn handle(&mut self, _: GetThread, _: &mut Self::Context) -> Self::Result {
        unimplemented!()
    }
}

/// Message used to fetch a specific post.
#[derive(Deserialize, Debug)]
pub struct GetPost { pub id: i32 }

message!(GetPost, Result<SimplePost>);

impl Handler<GetPost> for DbExecutor {
    type Result = <GetPost as Message>::Result;

    fn handle(&mut self, _: GetPost, _: &mut Self::Context) -> Self::Result {
        unimplemented!()
    }
}

/// Message used to update the content of a post.
#[derive(Deserialize)]
pub struct UpdatePost {
    pub post_id: i32,
    pub post: String,
}

message!(UpdatePost, Result<Post>);

impl Handler<UpdatePost> for DbExecutor {
    type Result = Result<Post>;

    fn handle(&mut self, _: UpdatePost, _: &mut Self::Context) -> Self::Result {
        unimplemented!()
    }
}

/// Message used to create a new thread
pub struct CreateThread {
    pub new_thread: NewThread,
    pub post: String,
}
message!(CreateThread, Result<Thread>);

impl Handler<CreateThread> for DbExecutor {
    type Result = <CreateThread as Message>::Result;

    fn handle(&mut self, _: CreateThread, _: &mut Self::Context) -> Self::Result {
        unimplemented!()
    }
}

/// Message used to create a new reply
pub struct CreatePost(pub NewPost);
message!(CreatePost, Result<Post>);

impl Handler<CreatePost> for DbExecutor {
    type Result = <CreatePost as Message>::Result;

    fn handle(&mut self, _: CreatePost, _: &mut Self::Context) -> Self::Result {
        unimplemented!()
    }
}

/// Message used to search for posts
#[derive(Deserialize)]
pub struct SearchPosts { pub query: String }
message!(SearchPosts, Result<Vec<SearchResult>>);

impl Handler<SearchPosts> for DbExecutor {
    type Result = <SearchPosts as Message>::Result;

    fn handle(&mut self, _: SearchPosts, _: &mut Self::Context) -> Self::Result {
        unimplemented!()
    }
}

/// Message that triggers a refresh of the view used for full-text
/// searching.
pub struct RefreshSearchView;
message!(RefreshSearchView, Result<()>);

impl Handler<RefreshSearchView> for DbExecutor {
    type Result = Result<()>;

    fn handle(&mut self, _: RefreshSearchView, _: &mut Self::Context) -> Self::Result {
        unimplemented!()
    }
}
