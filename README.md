# hasklet-store

Versioned NoSQL content store.

## Features

* Rest API
* Space efficient - only changes are persisted when an item is updated
* Auditable - the history of every item is persisted
* No dependencies - uses sqlite internally 

## API

Stored content items have the following form:

    { 
      id :: UUID,
      contentType :: String,
      active :: Boolean,
      lastModifiedTime :: DateTime,
      createdTime :: DateTime,
      fields :: JSON'
    }

`UUID` is a string in the following format: `423791df-9dec-4645-a41e-2bc3d7fffbe9`
`contentType` is an arbitrary string.
`active` specifies if an item has been deleted or not
`fields` is _almost_ arbitrary JSON. To simplify the persistence of fields, JSON arrays are not allowed and the '.' character is not allowed in key names.
`DateTime`s are strings in the following format: `YYYY-MM-DDTHH:MM:SS.SSSZ`

### POST /content
Body should contain a content item minus the `lastModifiedTime` and `createdTime` fields.
This stores the content.

### PUT /content/:id
Body should contain an `item` minus the `lastModifiedTime` and `createdTime` fields.
This updates or creates the item with an id that matches `:id`. This is idempotent.

### GET /content
Lists all content
#### Query Parameters
`type` only return content matching this content type
`active` only return content in the specified state
`time` return content as it was at the time specified
`contentId` only return content with ids after the specified id
`limit` only return at most, this many content items

### GET /content/:id
Get a content item by id.
#### Query Parameters
`time` return content as it was at the time specified

### GET /content/:id/versions
This returns a list of `DateTimes`.

## Building

The project uses `stack` so use `stack build` to build `stack test` to test etc.

## Running

Data is stored in `Sqlite` internally so there is no need to set up a database. To run:

    hasklet-store [-p|--port PORT] [-d|--database DATABASE_PATH]

`DATABASE_PATH` is the path to the sqlite database; if it is missing, a new database is created.
