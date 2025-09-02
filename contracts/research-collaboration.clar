
;; title: research-collaboration
;; version: 1.0.0
;; summary: Academic partnership system with project coordination, funding distribution, and IP management
;; description: Smart contract for managing research collaborations, partnerships, funding, and intellectual property rights

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROJECT-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-MEMBER (err u102))
(define-constant ERR-NOT-MEMBER (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-INVALID-PERCENTAGE (err u105))
(define-constant ERR-PROJECT-COMPLETED (err u106))
(define-constant ERR-INVALID-STATUS (err u107))
(define-constant ERR-IP-ALREADY-EXISTS (err u108))

;; Status constants
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-COMPLETED u2)
(define-constant STATUS-CANCELLED u3)

;; Contract owner
(define-data-var contract-owner principal tx-sender)
(define-data-var project-counter uint u0)
(define-data-var ip-counter uint u0)

;; Project data structure
(define-map projects
  uint
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    lead-researcher: principal,
    funding-target: uint,
    funding-raised: uint,
    status: uint,
    created-at: uint,
    completion-date: (optional uint)
  }
)

;; Project members and their roles
(define-map project-members
  { project-id: uint, member: principal }
  {
    role: (string-ascii 50),
    contribution-percentage: uint,
    joined-at: uint
  }
)

;; Funding contributions
(define-map funding-contributions
  { project-id: uint, contributor: principal }
  uint
)

;; Intellectual Property registry
(define-map intellectual-property
  uint
  {
    project-id: uint,
    title: (string-ascii 100),
    description: (string-ascii 300),
    ip-type: (string-ascii 20),
    creator: principal,
    created-at: uint
  }
)

;; IP ownership shares
(define-map ip-ownership
  { ip-id: uint, owner: principal }
  uint
)

;; Publications registry
(define-map publications
  { project-id: uint, publication-id: uint }
  {
    title: (string-ascii 150),
    authors: (list 10 principal),
    publication-date: uint,
    journal: (string-ascii 100)
  }
)

;; Create a new research project
(define-public (create-project (title (string-ascii 100)) (description (string-ascii 500)) (funding-target uint))
  (let
    ((project-id (+ (var-get project-counter) u1)))
    (map-set projects project-id
      {
        title: title,
        description: description,
        lead-researcher: tx-sender,
        funding-target: funding-target,
        funding-raised: u0,
        status: STATUS-ACTIVE,
        created-at: stacks-block-height,
        completion-date: none
      }
    )
    (map-set project-members
      { project-id: project-id, member: tx-sender }
      {
        role: "Lead Researcher",
        contribution-percentage: u100,
        joined-at: stacks-block-height
      }
    )
    (var-set project-counter project-id)
    (ok project-id)
  )
)

;; Add a member to a research project
(define-public (add-project-member (project-id uint) (member principal) (role (string-ascii 50)) (contribution-percentage uint))
  (let
    ((project (unwrap! (map-get? projects project-id) ERR-PROJECT-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get lead-researcher project)) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? project-members { project-id: project-id, member: member })) ERR-ALREADY-MEMBER)
    (asserts! (and (> contribution-percentage u0) (<= contribution-percentage u100)) ERR-INVALID-PERCENTAGE)
    (asserts! (is-eq (get status project) STATUS-ACTIVE) ERR-PROJECT-COMPLETED)
    (map-set project-members
      { project-id: project-id, member: member }
      {
        role: role,
        contribution-percentage: contribution-percentage,
        joined-at: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Contribute funding to a project
(define-public (contribute-funding (project-id uint) (amount uint))
  (let
    ((project (unwrap! (map-get? projects project-id) ERR-PROJECT-NOT-FOUND))
     (current-contribution (default-to u0 (map-get? funding-contributions { project-id: project-id, contributor: tx-sender }))))
    (asserts! (is-eq (get status project) STATUS-ACTIVE) ERR-PROJECT-COMPLETED)
    (asserts! (> amount u0) ERR-INSUFFICIENT-FUNDS)
    ;; In a real implementation, you would handle STX transfer here
    (map-set funding-contributions
      { project-id: project-id, contributor: tx-sender }
      (+ current-contribution amount)
    )
    (map-set projects project-id
      (merge project { funding-raised: (+ (get funding-raised project) amount) })
    )
    (ok true)
  )
)

;; Register intellectual property
(define-public (register-ip (project-id uint) (title (string-ascii 100)) (description (string-ascii 300)) (ip-type (string-ascii 20)))
  (let
    ((project (unwrap! (map-get? projects project-id) ERR-PROJECT-NOT-FOUND))
     (ip-id (+ (var-get ip-counter) u1))
     (member-info (unwrap! (map-get? project-members { project-id: project-id, member: tx-sender }) ERR-NOT-MEMBER)))
    (map-set intellectual-property ip-id
      {
        project-id: project-id,
        title: title,
        description: description,
        ip-type: ip-type,
        creator: tx-sender,
        created-at: stacks-block-height
      }
    )
    (map-set ip-ownership
      { ip-id: ip-id, owner: tx-sender }
      (get contribution-percentage member-info)
    )
    (var-set ip-counter ip-id)
    (ok ip-id)
  )
)

;; Add publication to project
(define-public (add-publication (project-id uint) (publication-id uint) (title (string-ascii 150)) (authors (list 10 principal)) (journal (string-ascii 100)))
  (let
    ((project (unwrap! (map-get? projects project-id) ERR-PROJECT-NOT-FOUND))
     (member-info (unwrap! (map-get? project-members { project-id: project-id, member: tx-sender }) ERR-NOT-MEMBER)))
    (map-set publications
      { project-id: project-id, publication-id: publication-id }
      {
        title: title,
        authors: authors,
        publication-date: stacks-block-height,
        journal: journal
      }
    )
    (ok true)
  )
)

;; Complete a project
(define-public (complete-project (project-id uint))
  (let
    ((project (unwrap! (map-get? projects project-id) ERR-PROJECT-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get lead-researcher project)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status project) STATUS-ACTIVE) ERR-INVALID-STATUS)
    (map-set projects project-id
      (merge project {
        status: STATUS-COMPLETED,
        completion-date: (some stacks-block-height)
      })
    )
    (ok true)
  )
)

;; Get project information
(define-read-only (get-project (project-id uint))
  (map-get? projects project-id)
)

;; Get project member information
(define-read-only (get-project-member (project-id uint) (member principal))
  (map-get? project-members { project-id: project-id, member: member })
)

;; Get funding contribution
(define-read-only (get-funding-contribution (project-id uint) (contributor principal))
  (default-to u0 (map-get? funding-contributions { project-id: project-id, contributor: contributor }))
)

;; Get intellectual property information
(define-read-only (get-ip-info (ip-id uint))
  (map-get? intellectual-property ip-id)
)

;; Get IP ownership percentage
(define-read-only (get-ip-ownership (ip-id uint) (owner principal))
  (default-to u0 (map-get? ip-ownership { ip-id: ip-id, owner: owner }))
)

;; Get publication information
(define-read-only (get-publication (project-id uint) (publication-id uint))
  (map-get? publications { project-id: project-id, publication-id: publication-id })
)

;; Get current project counter
(define-read-only (get-project-count)
  (var-get project-counter)
)

;; Get current IP counter
(define-read-only (get-ip-count)
  (var-get ip-counter)
)
