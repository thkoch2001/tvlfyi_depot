CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
-- ;;
CREATE TABLE "attendee" (
    "id" UUID PRIMARY KEY NOT NULL DEFAULT uuid_generate_v4(),
    "meetup_name" TEXT NOT NULL,
    "discord_name" TEXT,
    "meetup_user_id" TEXT,
    "organizer_notes" TEXT NOT NULL DEFAULT '',
    "created_at" TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT now()
);
-- ;;
CREATE TABLE "event" (
    "id" UUID PRIMARY KEY NOT NULL DEFAULT uuid_generate_v4(),
    "date" DATE NOT NULL,
    "created_at" TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT now()
);
-- ;;
CREATE TABLE "event_attendee" (
    "event_id" UUID NOT NULL REFERENCES "event" ("id"),
    "attendee_id" UUID NOT NULL REFERENCES "attendee" ("id"),
    "rsvpd_attending" BOOL,
    "attended" BOOL,
    "created_at" TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT now(),
    PRIMARY KEY ("event_id", "attendee_id")
);
-- ;;
CREATE TABLE "user" (
    "id" UUID PRIMARY KEY NOT NULL DEFAULT uuid_generate_v4(),
    "discord_user_id" TEXT NOT NULL,
    "created_at" TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT now()
);
