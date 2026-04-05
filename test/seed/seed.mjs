import nodemailer from "nodemailer";
import { faker } from "@faker-js/faker";
import { readFileSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";

const SMTP_HOST = process.env.SMTP_HOST || "localhost";
const SMTP_PORT = parseInt(process.env.SMTP_PORT || "3025");
const TARGET_USER = process.env.TARGET_USER || "testuser@localhost";

const transporter = nodemailer.createTransport({
  host: SMTP_HOST,
  port: SMTP_PORT,
  secure: false,
  tls: { rejectUnauthorized: false },
});

let messageCount = 0;

async function send(opts) {
  await transporter.sendMail(opts);
  messageCount++;
  process.stdout.write(`\rSent ${messageCount} emails...`);
}

// --- Seed functions ---

async function seedPlainText(count = 10) {
  for (let i = 0; i < count; i++) {
    await send({
      from: `${faker.person.fullName()} <${faker.internet.email()}>`,
      to: TARGET_USER,
      subject: faker.lorem.sentence(),
      text: faker.lorem.paragraphs(3),
      date: faker.date.recent({ days: 30 }),
    });
  }
}

async function seedHtmlOnly(count = 5) {
  for (let i = 0; i < count; i++) {
    await send({
      from: `${faker.person.fullName()} <${faker.internet.email()}>`,
      to: TARGET_USER,
      subject: `[HTML] ${faker.lorem.sentence()}`,
      html: `<html><body>
        <h1>${faker.lorem.sentence()}</h1>
        <p>${faker.lorem.paragraphs(2)}</p>
        <p style="color: blue;">${faker.lorem.paragraph()}</p>
      </body></html>`,
      date: faker.date.recent({ days: 30 }),
    });
  }
}

async function seedMultipartAlternative(count = 5) {
  for (let i = 0; i < count; i++) {
    const text = faker.lorem.paragraphs(2);
    await send({
      from: `${faker.person.fullName()} <${faker.internet.email()}>`,
      to: TARGET_USER,
      subject: `[Multi] ${faker.lorem.sentence()}`,
      text: text,
      html: `<html><body><p>${text.replace(/\n/g, "</p><p>")}</p></body></html>`,
      date: faker.date.recent({ days: 30 }),
    });
  }
}

async function seedWithAttachments(count = 5) {
  for (let i = 0; i < count; i++) {
    const attachments = [
      {
        filename: "document.txt",
        content: faker.lorem.paragraphs(5),
      },
    ];

    // Add a "binary" attachment (fake PDF header)
    if (i % 2 === 0) {
      attachments.push({
        filename: "report.pdf",
        content: Buffer.from(
          "%PDF-1.4 " + faker.lorem.paragraphs(10),
          "utf-8"
        ),
        contentType: "application/pdf",
      });
    }

    // Add an image attachment
    if (i % 3 === 0) {
      // 1x1 red PNG
      const pngBase64 =
        "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8/5+hHgAHggJ/PchI7wAAAABJRU5ErkJggg==";
      attachments.push({
        filename: "image.png",
        content: Buffer.from(pngBase64, "base64"),
        contentType: "image/png",
      });
    }

    await send({
      from: `${faker.person.fullName()} <${faker.internet.email()}>`,
      to: TARGET_USER,
      subject: `[Attachment] ${faker.lorem.sentence()}`,
      text: faker.lorem.paragraphs(1),
      attachments,
      date: faker.date.recent({ days: 30 }),
    });
  }
}

async function seedLargeAttachment() {
  const largeContent = Buffer.alloc(5 * 1024 * 1024, "A"); // 5MB
  await send({
    from: `${faker.person.fullName()} <${faker.internet.email()}>`,
    to: TARGET_USER,
    subject: "[Large] 5MB attachment test",
    text: "This email has a large attachment for testing.",
    attachments: [
      {
        filename: "large_file.bin",
        content: largeContent,
        contentType: "application/octet-stream",
      },
    ],
  });
}

async function seedThreads() {
  // Thread 1: 2-deep reply
  const thread1Id = `<${faker.string.uuid()}@test.local>`;
  await send({
    from: "alice@test.local",
    to: TARGET_USER,
    subject: "Lunch plans",
    text: "Want to grab lunch tomorrow?",
    messageId: thread1Id,
  });
  await send({
    from: TARGET_USER,
    to: "alice@test.local",
    subject: "Re: Lunch plans",
    text: "Sure! How about noon?",
    inReplyTo: thread1Id,
    references: thread1Id,
  });

  // Thread 2: 10-deep chain
  let parentId = `<${faker.string.uuid()}@test.local>`;
  let refs = parentId;
  await send({
    from: "bob@test.local",
    to: TARGET_USER,
    subject: "Project update",
    text: "Here's the latest on the project.",
    messageId: parentId,
  });

  for (let i = 1; i <= 9; i++) {
    const newId = `<${faker.string.uuid()}@test.local>`;
    const sender = i % 2 === 0 ? "bob@test.local" : TARGET_USER;
    const recipient = i % 2 === 0 ? TARGET_USER : "bob@test.local";
    await send({
      from: sender,
      to: recipient,
      subject: `Re: Project update`,
      text: faker.lorem.sentence(),
      messageId: newId,
      inReplyTo: parentId,
      references: refs,
    });
    refs = refs + " " + newId;
    parentId = newId;
  }

  // Thread 3: Reply with changed subject
  const thread3Id = `<${faker.string.uuid()}@test.local>`;
  await send({
    from: "charlie@test.local",
    to: TARGET_USER,
    subject: "Meeting agenda",
    text: "Let's discuss the Q2 roadmap.",
    messageId: thread3Id,
  });
  await send({
    from: TARGET_USER,
    to: "charlie@test.local",
    subject: "Re: Meeting agenda — also Q3 planning",
    text: "Can we add Q3 planning too?",
    inReplyTo: thread3Id,
    references: thread3Id,
  });
}

async function seedMultipleRecipients(count = 5) {
  for (let i = 0; i < count; i++) {
    await send({
      from: `${faker.person.fullName()} <${faker.internet.email()}>`,
      to: [TARGET_USER, faker.internet.email()].join(", "),
      cc: [faker.internet.email(), faker.internet.email()].join(", "),
      bcc: faker.internet.email(),
      subject: `[CC/BCC] ${faker.lorem.sentence()}`,
      text: faker.lorem.paragraphs(1),
      date: faker.date.recent({ days: 30 }),
    });
  }
}

async function seedEncodingEdgeCases() {
  // UTF-8 subject (Japanese)
  await send({
    from: "tanaka@test.local",
    to: TARGET_USER,
    subject: "日本語のテスト件名 — テスト",
    text: "この文章は日本語で書かれています。",
  });

  // Emoji in subject
  await send({
    from: "emoji@test.local",
    to: TARGET_USER,
    subject: "🎉 Celebration! 🎊 Party time 🥳",
    text: "Lots of emoji: 🌸🎌🗾",
  });

  // Arabic
  await send({
    from: "arabic@test.local",
    to: TARGET_USER,
    subject: "رسالة باللغة العربية",
    text: "هذه رسالة اختبار باللغة العربية",
  });

  // Display name with special characters
  await send({
    from: '"Doe, John (Jr.)" <john.doe@test.local>',
    to: TARGET_USER,
    subject: "Special chars in display name",
    text: "Testing special characters in From header.",
  });

  // Very long subject (500+ chars)
  await send({
    from: "long@test.local",
    to: TARGET_USER,
    subject: faker.lorem.words(100),
    text: "This email has a very long subject line.",
  });
}

async function seedHeaderEdgeCases() {
  // Missing subject
  await send({
    from: "nosubject@test.local",
    to: TARGET_USER,
    subject: "",
    text: "This email has no subject.",
  });

  // Empty body
  await send({
    from: "nobody@test.local",
    to: TARGET_USER,
    subject: "Empty body test",
    text: "",
  });

  // Very large body (100KB+)
  await send({
    from: "largebody@test.local",
    to: TARGET_USER,
    subject: "[Large body] 100KB+ text content",
    text: faker.lorem.paragraphs(200),
  });

  // HTML only with inline CSS, no text part
  await send({
    from: "htmlonly@test.local",
    to: TARGET_USER,
    subject: "[HTML only] Styled email",
    html: `<html>
      <head><style>body { font-family: Arial; } .highlight { background: yellow; }</style></head>
      <body>
        <h1 class="highlight">${faker.lorem.sentence()}</h1>
        <p>${faker.lorem.paragraphs(3)}</p>
      </body>
    </html>`,
  });
}

// --- Main ---

async function main() {
  console.log(`Seeding emails to ${TARGET_USER} via ${SMTP_HOST}:${SMTP_PORT}`);
  console.log("---");

  await seedPlainText(10);
  await seedHtmlOnly(5);
  await seedMultipartAlternative(5);
  await seedWithAttachments(5);
  await seedLargeAttachment();
  await seedThreads();
  await seedMultipleRecipients(5);
  await seedEncodingEdgeCases();
  await seedHeaderEdgeCases();

  console.log(`\nDone! Sent ${messageCount} emails total.`);
  process.exit(0);
}

main().catch((err) => {
  console.error("Seed failed:", err);
  process.exit(1);
});
