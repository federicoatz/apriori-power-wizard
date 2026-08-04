# Privacy notice

*Last updated: 4 August 2026.*

This notice describes what the **A Priori Power Analysis Wizard**
(<https://federicoatz.com/power-analysis-app/>) does with data. It is written to
be checkable: every claim below can be verified by opening your browser's
developer tools and watching the Network and Application tabs while you use the
application.

## Who is responsible

The data controller is **Federico Atzori**, University of Cagliari.

Contact: `federico.atzori2 [at] unica [dot] it`

(The address is written that way deliberately, to keep it out of the reach of
automated address harvesters. Replace `[at]` with `@` and `[dot]` with `.`)

## The short version

Nothing you type into this application is sent anywhere. Every calculation runs
locally, inside your own browser, using a WebAssembly build of R. There is no
account, no login, no database, and no server that could receive your inputs,
because the application is a set of static files.

## What the application does not do

- **It sets no cookies.** None at all.
- **It does not transmit your inputs.** The effect sizes, sample sizes, and
  study parameters you enter never leave your device. The same is true of any
  project file you load and any report you generate: those are produced in the
  browser and saved directly to your own computer.
- **It does not ask for, or store, any personal information.** You are never
  asked for a name, an email address, or an affiliation.
- **It contacts no third parties** other than the analytics service described
  below. The webfont it uses is served from the same site rather than from an
  external font CDN, specifically so that your IP address is not disclosed to a
  third party simply by opening the page.

## What is stored on your device

If, and only if, you actively switch the colour theme or turn on Guided mode,
that single preference is saved in your browser's `localStorage` so the
application can remember it on your next visit. Two small values are involved
(`pw-theme` and `pw-guided`). They contain nothing but your chosen setting, are
never transmitted, and can be removed at any time by clearing site data in your
browser. If you never touch either control, nothing is stored.

The "share link" feature encodes the values you entered into the page's own web
address so you can send it to a collaborator. That address is created in your
browser and is not recorded by us; be aware, though, that anything you put in a
link may be visible to whoever you send it to, and may appear in their browser
history.

## Usage statistics

To understand roughly how much the tool is used — information that matters for
reporting on and sustaining an academic project — the site uses
[GoatCounter](https://www.goatcounter.com/), an open-source analytics service.

GoatCounter is used here in its default configuration, which:

- sets **no cookies** and does not track you across sites;
- does **not** store IP addresses or any other information that identifies you
  individually;
- records only aggregate information such as the page visited, the approximate
  time, the referring site, and a coarse browser/country category.

GoatCounter's own privacy policy is available at
<https://www.goatcounter.com/help/privacy>.

## Hosting

The site is hosted on **GitHub Pages** (GitHub, Inc., a Microsoft company). As
with any web host, GitHub's servers process the technical information necessary
to deliver a page, including your IP address, and may keep access logs. These
logs are held by GitHub and are not accessible to the controller of this site.
GitHub's privacy statement is available at
<https://docs.github.com/en/site-policy/privacy-policies/github-privacy-statement>.

## Your rights

Under the GDPR you have the right to access, rectify, or erase personal data
concerning you, to restrict or object to its processing, and to lodge a
complaint with a supervisory authority (in Italy, the Garante per la protezione
dei dati personali, <https://www.garanteprivacy.it/>).

In practice this application holds no personal data that could be retrieved or
deleted on request: the settings described above are on your own device and
under your own control, and the usage statistics are aggregate and not linked to
you. Requests can nonetheless be sent to the contact address above.

## Changes

This notice will be updated if the application's behaviour changes. Its full
revision history is public, in the repository's commit log.

---

*This notice describes the software's actual behaviour, verified by inspecting
the deployed site's network traffic and storage. It is not legal advice.*
