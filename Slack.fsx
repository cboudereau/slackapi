#r "packages/FSharp.Data/lib/net45/FSharp.Data.dll"
open System
open System.IO
open System.Threading

module Log = 
    type [<Struct>] LogLevel = TRACE | DEBUG | INFO | WARN | ERROR
    let private value = function
        | ERROR -> 1
        | WARN  -> 2
        | INFO  -> 3
        | DEBUG -> 4
        | TRACE -> 5
    let private pretty = function
        | ERROR -> "[ERR]"
        | WARN  -> "[WRN]"
        | INFO  -> "[INF]"
        | DEBUG -> "[DBG]"
        | TRACE -> "[TRC]"

    let mutable private logLevel = None
    let setLogLevel lvl = logLevel <- lvl

    let mutable Logger = 
        fun lvl (x:string) -> 
            sprintf "%s : %s"(pretty lvl) x |> Console.WriteLine

    let internal log lvl = 
        fun fmt -> 
            let logger = 
                let logLvl = logLevel |> Option.map value |> Option.defaultValue 0
                let v = value lvl
                if v > logLvl  then ignore else Logger lvl
            Printf.ksprintf logger fmt

module Async = 
    let map f x = async { let! x' = x in return f x' }
    let bind f x =  async.Bind(x, f)
    let ret x = async.Return(x)
    let ofOption zero = function Some x -> x | None -> ret zero

module Parsing = 
    open System.Text.RegularExpressions
    let (|Regex|_|) pattern source = 
        let m = Regex.Match(source, pattern, RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase)
        if m.Success then [ for x in m.Groups -> x.Value ] |> List.tail |> Some
        else None

module String = 
    let icontains search source = (source:string).IndexOf((search:string), StringComparison.InvariantCultureIgnoreCase) <> -1
    let (|Contains|_|) search source =
        if icontains search source then Some source
        else None 

type [<Struct>] TeamMember = TeamMember of string
type [<Struct>] Email = Email of string
type [<Struct>] TextMessage = TextMessage of string
type [<Struct>] Channel = Channel of string
type [<Struct>] Bot = Bot of string

type SlackBotException (m,i) = inherit exn ((m:string),(i:exn))

open FSharp.Data
open System.Net.WebSockets

let [<Literal>] private rtmStartJson = """
{
  "ok": true,
  "self": {
    "id": "Toto",
    "name": "Toto",
    "prefs": {
      "highlight_words": "",
      "user_colors": "",
      "color_names_in_list": true,
      "growls_enabled": true,
      "tz": null,
      "push_dm_alert": true,
      "push_mention_alert": true,
      "msg_replies": "\"flexpane\":false }",
      "push_everything": false,
      "push_idle_wait": 2,
      "push_sound": "Toto",
      "push_loud_channels": "",
      "push_mention_channels": "",
      "push_loud_channels_set": "",
      "email_alerts": "Toto",
      "email_alerts_sleep_until": 0,
      "email_misc": true,
      "email_weekly": true,
      "welcome_message_hidden": false,
      "all_channels_loud": false,
      "loud_channels": "",
      "never_channels": "",
      "loud_channels_set": "",
      "show_member_presence": true,
      "search_sort": "Toto",
      "expand_inline_imgs": true,
      "expand_internal_inline_imgs": true,
      "expand_snippets": false,
      "posts_formatting_guide": true,
      "seen_live_support_popup": false,
      "seen_welcome_2": false,
      "seen_ssb_prompt": false,
      "seen_spaces_new_xp_tooltip": false,
      "spaces_new_xp_banner_dismissed": false,
      "search_only_my_channels": false,
      "emoji_mode": "Toto",
      "emoji_use": "",
      "has_invited": false,
      "has_uploaded": false,
      "has_created_channel": false,
      "search_exclude_channels": "",
      "messages_theme": "Toto",
      "webapp_spellcheck": true,
      "no_joined_overlays": false,
      "no_created_overlays": false,
      "dropbox_enabled": false,
      "seen_domain_invite_reminder": false,
      "seen_member_invite_reminder": false,
      "mute_sounds": false,
      "arrow_history": false,
      "tab_ui_return_selects": true,
      "obey_inline_img_limit": true,
      "new_msg_snd": "Toto",
      "collapsible": false,
      "collapsible_by_click": true,
      "require_at": true,
      "ssb_space_window": "",
      "mac_ssb_bounce": "",
      "mac_ssb_bullet": true,
      "expand_non_media_attachments": true,
      "show_typing": true,
      "pagekeys_handled": true,
      "last_snippet_type": "",
      "display_real_names_override": 0,
      "display_preferred_names": true,
      "time24": false,
      "enter_is_special_in_tbt": false,
      "graphic_emoticons": false,
      "convert_emoticons": true,
      "autoplay_chat_sounds": true,
      "ss_emojis": true,
      "sidebar_behavior": "",
      "seen_onboarding_start": false,
      "onboarding_cancelled": false,
      "seen_onboarding_slackbot_conversation": false,
      "seen_onboarding_channels": false,
      "seen_onboarding_direct_messages": false,
      "seen_onboarding_invites": false,
      "seen_onboarding_search": false,
      "seen_onboarding_recent_mentions": false,
      "seen_onboarding_starred_items": false,
      "seen_onboarding_private_groups": false,
      "onboarding_slackbot_conversation_step": 0,
      "dnd_enabled": false,
      "dnd_start_hour": "Toto",
      "dnd_end_hour": "Toto",
      "mark_msgs_read_immediately": true,
      "start_scroll_at_oldest": true,
      "snippet_editor_wrap_long_lines": false,
      "ls_disabled": false,
      "sidebar_theme": "Toto",
      "sidebar_theme_custom_values": "",
      "f_key_search": false,
      "k_key_omnibox": true,
      "speak_growls": false,
      "mac_speak_voice": "Toto",
      "mac_speak_speed": 250,
      "comma_key_prefs": false,
      "at_channel_suppressed_channels": "",
      "push_at_channel_suppressed_channels": "",
      "prompted_for_email_disabling": false,
      "full_text_extracts": false,
      "no_text_in_notifications": false,
      "muted_channels": "",
      "no_macssb1_banner": false,
      "no_macssb2_banner": false,
      "no_winssb1_banner": false,
      "no_omnibox_in_channels": false,
      "k_key_omnibox_auto_hide_count": 0,
      "hide_user_group_info_pane": false,
      "mentions_exclude_at_user_groups": false,
      "privacy_policy_seen": true,
      "search_exclude_bots": false,
      "load_lato_2": false,
      "fuller_timestamps": false,
      "last_seen_at_channel_warning": 0,
      "flex_resize_window": false,
      "msg_preview": false,
      "msg_preview_displaces": true,
      "msg_preview_persistent": true,
      "emoji_autocomplete_big": false,
      "winssb_run_from_tray": true,
      "winssb_window_flash_behavior": "Toto",
      "two_factor_auth_enabled": false,
      "two_factor_type": null,
      "two_factor_backup_type": null,
      "mentions_exclude_at_channels": true,
      "confirm_clear_all_unreads": true,
      "confirm_user_marked_away": true,
      "box_enabled": false,
      "seen_single_emoji_msg": false,
      "confirm_sh_call_start": true,
      "preferred_skin_tone": "",
      "show_all_skin_tones": false,
      "separate_private_channels": false,
      "whats_new_read": 1460711946,
      "hotness": false,
      "frecency_jumper": "",
      "jumbomoji": true,
      "no_flex_in_history": false,
      "newxp_seen_last_message": 0
    },
    "created": 1460711946,
    "manual_presence": "Toto"
  },
  "team": {
    "id": "Toto",
    "name": "Toto",
    "email_domain": "Toto",
    "domain": "Toto",
    "msg_edit_window_mins": -1,
    "prefs": {
      "default_channels": [ "C025RN54A", "C025RN54N" ],
      "msg_edit_window_mins": -1,
      "allow_message_deletion": true,
      "hide_referers": false,
      "display_real_names": true,
      "who_can_at_everyone": "Toto",
      "who_can_at_channel": "Toto",
      "who_can_create_channels": "Toto",
      "who_can_archive_channels": "Toto",
      "who_can_create_groups": "Toto",
      "who_can_post_general": "Toto",
      "who_can_kick_channels": "Toto",
      "who_can_kick_groups": "Toto",
      "retention_type": 0,
      "retention_duration": 0,
      "group_retention_type": 0,
      "group_retention_duration": 0,
      "dm_retention_type": 0,
      "dm_retention_duration": 0,
      "require_at_for_mention": 0,
      "auth_mode": "Toto",
      "posts_migrating": 0,
      "allow_calls": false,
      "who_can_create_shared_channels": "Toto",
      "file_retention_duration": 0,
      "file_retention_type": 0,
      "allow_retention_override": true,
      "compliance_export_start": 0,
      "warn_before_at_channel": "Toto",
      "disallow_public_file_urls": false,
      "who_can_create_delete_user_groups": "Toto",
      "who_can_edit_user_groups": "Toto",
      "who_can_change_team_profile": "Toto",
      "allow_shared_channels": false,
      "who_has_team_visibility": "Toto",
      "invites_only_admins": true,
      "disable_file_uploads": "Toto",
      "dnd_enabled": true,
      "dnd_start_hour": "Toto",
      "dnd_end_hour": "Toto",
      "who_can_manage_integrations": { "type": [ "regular" ] }
    },
    "icon": {
      "image_34": "Toto",
      "image_44": "Toto",
      "image_68": "Toto",
      "image_88": "Toto",
      "image_102": "Toto",
      "image_132": "Toto",
      "image_original": "Toto"
    },
    "over_storage_limit": false,
    "plan": "",
    "over_integrations_limit": true
  },
  "latest_event_ts": "Toto",
  "channels": [
    {
      "id": "Toto",
      "name": "Toto",
      "is_channel": true,
      "created": 1460714443,
      "creator": "Toto",
      "is_archived": false,
      "is_general": false,
      "has_pins": false,
      "is_member": true,
      "last_read": "Toto",
      "latest": {
        "type": "Toto",
        "user": "Toto",
        "text": "Toto",
        "ts": "Toto"
      },
      "unread_count": 9,
      "unread_count_display": 5,
      "members": [ "U02KVK26U", "U10V5V41W" ],
      "topic": {
        "value": "",
        "creator": "",
        "last_set": 0
      },
      "purpose": {
        "value": "",
        "creator": "",
        "last_set": 0
      }
    },
    {
      "id": "Toto",
      "name": "Toto",
      "is_channel": true,
      "created": 1405005521,
      "creator": "Toto",
      "is_archived": false,
      "is_general": false,
      "has_pins": false,
      "is_member": false
    }
  ],
  "groups": [],
  "ims": [
    {
      "id": "Toto",
      "is_im": true,
      "user": "Toto",
      "created": 1460711946,
      "is_org_shared": false,
      "has_pins": false,
      "last_read": "Toto",
      "latest": null,
      "unread_count": 0,
      "unread_count_display": 0,
      "is_open": true
    }
  ],
  "cache_ts": 1460733507,
  "subteams": {
    "self": [],
    "all": []
  },
  "dnd": {
    "dnd_enabled": false,
    "next_dnd_start_ts": 1,
    "next_dnd_end_ts": 1,
    "snooze_enabled": false
  },
  "users": [
    {
      "id": "Toto",
      "team_id": "Toto",
      "name": "Toto",
      "deleted": false,
      "status": null,
      "color": "Toto",
      "real_name": "Toto",
      "tz": "Toto",
      "tz_label": "Toto",
      "tz_offset": 7200,
      "profile": {
        "first_name": "Toto",
        "last_name": "Toto",
        "avatar_hash": "Toto",
        "real_name": "Toto",
        "real_name_normalized": "Toto",
        "email": "Toto",
        "image_24": "Toto",
        "image_32": "Toto",
        "image_48": "Toto",
        "image_72": "Toto",
        "image_192": "Toto",
        "image_512": "Toto",
        "fields": null
      },
      "is_admin": false,
      "is_owner": false,
      "is_primary_owner": false,
      "is_restricted": false,
      "is_ultra_restricted": false,
      "is_bot": false,
      "presence": "Toto"
    },
    {
      "id": "Toto",
      "team_id": "Toto",
      "name": "Toto",
      "deleted": false,
      "status": null,
      "color": "Toto",
      "real_name": "Toto",
      "tz": "Toto",
      "tz_label": "Toto",
      "tz_offset": 7200,
      "profile": {
        "first_name": "Toto",
        "last_name": "Toto",
        "avatar_hash": "Toto",
        "real_name": "Toto",
        "real_name_normalized": "Toto",
        "image_24": "Toto",
        "image_32": "Toto",
        "image_48": "Toto",
        "image_72": "Toto",
        "image_192": "Toto",
        "image_512": "Toto",
        "fields": null
      },
      "is_admin": false,
      "is_owner": false,
      "is_primary_owner": false,
      "is_restricted": false,
      "is_ultra_restricted": false,
      "is_bot": false,
      "presence": "Toto"
    }
  ],
  "cache_version": "Toto",
  "cache_ts_version": "Toto",
  "bots": [
    {
      "id": "Toto",
      "deleted": false,
      "name": "Toto",
      "icons": {
        "image_36": "Toto",
        "image_48": "Toto",
        "image_72": "Toto"
      }
    }
  ],
  "url": "Toto"
}"""

let [<Literal>] private rtmMessageJson = """
[
  {
    "id": 1234,
    "type": "ping",
    "reply_to": 1,
    "time": 1403299273342
  },
  {
    "id": 1,
    "type": "message",
    "channel": "C024BE91L",
    "user": "U023BECGF",
    "text": "Hello world"
  }
]"""

type [<Struct>] SlackUserId = SlackUserId of string
type private RtmStart = JsonProvider< rtmStartJson >
type private RtmMessage = JsonProvider< rtmMessageJson, SampleIsList=true >

let Token = 
    let token = System.Environment.GetEnvironmentVariable("SlackToken")
    if System.String.IsNullOrWhiteSpace token then failwith "SlackToken env var is not set." else token

let private rtmStart () = Http.RequestString("https://slack.com/api/rtm.start", query=["token", Token], httpMethod="GET") |> RtmStart.Parse
let private RtmStart = rtmStart ()

let post (Channel channel) (TextMessage text) = 
    Http.Request(
        "https://slack.com/api/chat.postMessage", 
        httpMethod = HttpMethod.Post,
        body = FormValues(
            [ ("token", Token)
              ("channel", channel)
              ("text", text)
              ("as_user", "true")
              ("link_names", "1") ]))
    |> ignore

let tryFindChannel (Channel channel) = RtmStart.Channels |> Array.tryFind(fun c -> c.Name = channel)

let tryFindUserName (SlackUserId userId) = RtmStart.Users |> Array.tryFind(fun u -> u.Id = userId) |> Option.map(fun u -> TeamMember u.Name)

let findBotId (Bot bot) = 
    let user = RtmStart.Users |> Array.tryFind(fun u -> u.Name = bot)
    user |> Option.map (fun x -> x.Id |> SlackUserId)

let tryFindUserId (Email email) = 
    RtmStart.Users
    |> Array.filter(fun u -> u.Profile.Email = Some email)
    |> Array.tryHead
    |> Option.map(fun u -> SlackUserId u.Id)

let hasUser (SlackUserId botId) (TextMessage m) = String.icontains (sprintf "<@%s>" botId) m

let rec private receive (ms: MemoryStream) token (webSocket: ClientWebSocket) = 
    async {
        let buffer = new ArraySegment<byte>(Array.zeroCreate(8192))
        let! res = webSocket.ReceiveAsync(buffer, token) |> Async.AwaitTask
        ms.Write(buffer.Array, buffer.Offset, res.Count)
        if res.EndOfMessage 
        then
            ms.Seek(0L, SeekOrigin.Begin) |> ignore
            return ms.ToArray() 
        else
            return! webSocket |> receive ms token
    }

type [<Struct>] Message = Message of Channel * TeamMember * TextMessage

let private readMessage token (webSocket: ClientWebSocket) (SlackUserId botId) =
    async {
        Log.log Log.TRACE "readMessage for botId:%s" botId
        use ms = new MemoryStream ()
        return! 
            receive ms token webSocket
            |> Async.Catch
            |> Async.map (function
                | Choice1Of2 data -> 
                    let message = System.Text.ASCIIEncoding.ASCII.GetString(data) |> RtmMessage.Parse
                    match message.Type, message.ReplyTo, message.Text, message.User, message.Channel with
                    | String.Contains "pong" _, Some id, _, _, _ -> 
                        Log.log Log.DEBUG "pong %i" id
                        None
                    | String.Contains "message" _, _, Some (String.Contains botId text), Some user, Some channel when user <> botId ->
                        let userName = 
                            match user |> SlackUserId |> tryFindUserName with
                            | Some name -> name
                            | None -> TeamMember String.Empty
                        Some (Message (Channel channel, userName, TextMessage text))
                    | _ -> 
                        Log.log Log.DEBUG "message skipped : %A" message
                        None
                | Choice2Of2 ex -> 
                    Log.log Log.ERROR "readMessage unhandled exception %s with ex:%O" ex.Message ex
                    SlackBotException(sprintf "readMessage unhandled exception %s" ex.Message, ex) |> raise) }

let private sendData token (webSocket: ClientWebSocket) (data: string) = 
    async {
        let buffer = new ArraySegment<byte>(System.Text.ASCIIEncoding.ASCII.GetBytes(data))
        do! webSocket.SendAsync(buffer, WebSocketMessageType.Text, true, token) |> Async.AwaitTask
    }

let private ping token n webSocket = 
    async {
        Log.log Log.TRACE "ping"
        do! sprintf """{"id": %d, "type":"ping"}""" n |> sendData token webSocket
        Log.log Log.TRACE "ping sent"
    }

let private webSocketServer token userId handler = 
    let handle = function Message (c, tm, m) -> handler c tm m

    let rec read token (webSocket:ClientWebSocket) = 
        async {
            match webSocket.State with
            | WebSocketState.Open -> 
                do! readMessage token webSocket userId |> Async.bind (Option.map handle >> Async.ofOption ())
                do! read token webSocket
            | _ -> return ()
        }
    
    async { 
        use webSocket = new ClientWebSocket()
        Log.log Log.TRACE "socket connecting"
        let rtmStart = rtmStart ()

        let rec pinG token n (webSocket:ClientWebSocket) =
            async {
                match webSocket.State with
                | WebSocketState.Open ->
                    let next = if n = Int32.MaxValue then 1 else n + 1
                    do! ping token next webSocket
                    do! Async.Sleep 1000
                    do! pinG token next webSocket 
                | _ -> return ()
            }

        Log.log Log.INFO "rtmStart : %s" rtmStart.Url
        do! webSocket.ConnectAsync(Uri(rtmStart.Url), token) |> Async.AwaitTask
        do!
            [ read token webSocket
              pinG token 1 webSocket ]
            |> Async.Parallel
            |> Async.Ignore

        return webSocket.State
    }

let rec faultTolerantServer (cancelToken:CancellationToken) userId handler = 
    let next = 
        let rnd = Random ()
        fun () -> rnd.Next(0, 10)
    
    let reconnect () =
        if not cancelToken.IsCancellationRequested then 
            let time = next ()
            Log.log Log.WARN "retrying in %is" time
            time * 1000 |> Async.Sleep |> Async.bind (fun _ -> faultTolerantServer cancelToken userId handler)
        else 
            Log.log Log.TRACE "socketServer finished"
            Async.ret ()

    async {
        do! 
            webSocketServer cancelToken userId handler
            |> Async.Catch
            |> Async.bind (function 
                | Choice1Of2 WebSocketState.Open -> Log.log Log.INFO "server finished" |> Async.ret
                | Choice1Of2 state -> 
                    state.ToString() |> Log.log Log.WARN "server finished not gracefully with state %s"
                    reconnect ()
                | Choice2Of2 ex ->
                    Log.log Log.ERROR "reconnecting due to %O" ex
                    reconnect ())
    }
module Actor = 
    let spawn handler state = 
        let actor = 
            MailboxProcessor.Start <| fun channel -> 
                let rec listen ctx = 
                    async {
                        let! msg = channel.Receive ()
                        let! ctx = handler msg ctx
                        do! listen ctx
                    }
                listen state
        actor.Post