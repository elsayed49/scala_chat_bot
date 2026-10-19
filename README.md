GitHub Repo: [https://github.com/oppadmaged/productivity-chatbot].

# Running the Web Interface

## What you need

- Java 17+
- sbt installed
- A browser

---

## Steps

**1. Clone the repo and open a terminal in the project root.**

**2. Start the backend server:**

```bash
sbt "runMain server.ChatServer"
```

Wait until you see:

```
║  ProBot server running on port 8080      ║
```

The terminal will hang, that is correct. Do not close it.

**3. Open the frontend.**

Go to the `frontend/` folder and open `index.html` in your browser.
Use Live Server in VS Code, or just double-click the file.

**4. Start chatting.**

The sidebar has category shortcuts. You can also type freely in the input box.

**5. To stop the server:**

Go back to the terminal and press `Ctrl+C`.

---

## If the browser says "Could not reach the server"

- The server is not running. Go back to step 2.
- The terminal returned the prompt after printing the banner. That means the server crashed, re-run step 2.
- You are on a different port. The server must be on `8080`. Check nothing else is using that port.

---

## Notes

- Each browser tab gets its own session automatically.
- Refreshing the page keeps your session (stored in browser localStorage).
- Clicking "Clear conversation" in the sidebar starts a fresh session.
- The CLI (`sbt "runMain Main"`) still works independently — the server does not affect it.
# scala_chat_bot
