# sorge-config NEXT.md

> 다음 세션의 책갈피. 기존 운영 모델은 [AGENTS.md](./AGENTS.md), [README.md](./README.md), [ROADMAP.md](./ROADMAP.md)에 남긴다. 이름 변경을 역할 전환·이관 완료로 읽지 않는다.

# RAIL — 현재 좌표

- [x] **1. forge-config → sorge-config 이름 변경** — 현재 origin과 디렉터리 확인.
- [x] **2. 전환 방향·소유 경계 인계** — 이번 세션은 NEXT 정리만; 구현하지 않음.
- [ ] **3. 공개 측 역할 전환 범위 확정** ← CURRENT: 다음 GLG 재개 요청 때 현행 계약과 rename 잔여부터 읽기.
- [ ] **4. 승인된 수선·일반 절차 이관 및 검증** ← PAUSED: 범위·실행 승인 전에는 이관/설치/활성화하지 않음.

# NOW — forge 중심 공방에서 sorge 공개 연결면으로

- Current: 리포 이름은 `sorge-config`지만 본문·CLI·스킬은 아직 기존 forge 운영 모델이다. `sorge`로 이 리포를 다시 개명하는 일이 아니라, private sorge와 짝을 이루는 공개 설정/절차 자리로 전환할 준비다.
- Next: GLG가 재개하면 (1) 아래 Read와 실제 링크/설정을 읽어 현황 재측정 → (2) rename 수선과 역할 전환을 분리해 최소 변경 범위 제안 → (3) GLG와 공개 범위를 정한 뒤 실행.
- Verify: 승인된 변경 후 단일 스킬 실물·하네스 진입점·CLI 예문 경로가 일치하는지 확인. credential helper 경로 존재와 실제 인증 검증은 별도 축으로 기록하고, 링크 복구를 인증 성공으로 승격하지 않는다.
- Blocker: 환경 차단이 아니라 다음 실행 범위 승인 대기. 이번 요청은 NEXT 문서의 commit/push만.
- Read: `AGENTS.md`, `README.md`, `.claude/skills/forge/SKILL.md`, `bin/forge`, `bin/git-credential-forge`, `.github/agents/forge-config.agent.md`; 필요시 private 측 담당 코디에게 최신 계약을 다시 요청.
- Do not touch: 비공개 운영 데이터·실제 대상 목록·엔드포인트·인증 정보·대화 원문을 공개 issue/commit/NEXT로 복사하지 않는다. private 이력 통째 이관, 별도 스킬 사본, 자동 전달/구현/merge, 전역설정 일괄변경 금지.

## 다음 범위 협의 — 세 갈래를 섞지 않는다

1. **Rename 잔여 수선:** 이 집 스킬의 옛 `~/repos/gh/forge-config/bin/...` 경로와 소유권 설명, 현재 문서·페르소나 참조를 조사한다. 역사 문서/옛 remote 키는 일괄치환하지 않는다. `~/.gitconfig` credential helper는 머신 로컬 별도 조치이며 다른 리포 문서는 해당 owner 경계다.
2. **공개/비공개 소유 경계:** private sorge는 실제 요청·판정·운영 데이터를, public sorge-config는 재사용 가능한 절차·연결 실물 하나를 소유하는 방향을 협의한다. code/docs가 아니라 공개 절차/비공개 데이터로 가른다. 기존 forge CLI·라벨·footer·Copilot 안내는 채택/유지/정리 범위를 먼저 결정한다.
3. **이관 후보만 검토:** 신호·업무·권한 분리, 기존 대화에 공식 channel을 붙이는 절차, metadata-only lint 계약 등이 후보다. 구현 완료/이관 승인이라는 뜻은 아니다. 실물과 pointer의 소유권부터 정하고 필요한 최소 부분만 받는다.

# RECENT — 2026-10-31

- **직접 확인:** 이 세션 cwd는 `sorge-config`, origin은 `https://github.com/junghan0611/sorge-config.git`. NEXT 수정 전 main `b2bf408`, tree clean. GLG가 이름을 직접 바꿨다고 알렸고, 이번에는 구현 없이 전환 준비를 NEXT에 남겨 commit/push하라고 요청했다.
- **agent-config 코디 인계, 이 집에서 재측정하지 않음:** root `skills/forge`를 `../../sorge-config/.claude/skills/forge`로 수선하고 clone/skill 매핑·공개 remote registry·현재 문서를 갱신. 6개 하네스 연결과 관련 합성검사 통과 보고. 인계 당시 미커밋이며 이후 commit/push 상태는 미확인. 이 집 스킬 본문과 머신 로컬 helper 경로는 수선하지 않았다고 명시했다.
- **sorge 코디 마무리 인계, 이 집에서 재측정하지 않음:** private 측 hook/channel 작업과 검증을 정리했으며, 새 실제 업무 이벤트의 전체 왕복 실증과 일반 lint 구현/연결은 남아 있다. 재사용 절차의 공개 이관은 아직 하지 않았고 GLG가 다시 열 때 범위를 정한다. private 운영 영수증은 이 공개 문서에 옮기지 않는다.

# 유지할 기존 계약

- `forge` 스킬 실물은 CLI 옆 `.claude/skills/forge/SKILL.md` 한 벌, agent-config는 연결/노출 담당. CLI 계약 변경 시 스킬도 함께 갱신한다.
- webhook은 wake signal일 뿐 실행 권한이 아니다. 현재 원장 상태가 우선하며 Forgejo triage는 lifecycle set이 정확히 `{agent:ready}`일 때만 처리한다.
- `agent:done`은 1차 검토/분류 완료이지 구현 완료가 아니다. forgebot은 dispatcher/recorder이며 merge는 사람 게이트다.
- `auto-fix`는 패치 후보와 검증 루프이지 조용한 해결이 아니다. `label-set`, 장문 `comment --body-file`, mutating stderr observability를 유지한다.
- 기존 auto-fix 확장·label bootstrap·profile 확장·footer·배치 분류 과제는 현행 README/ROADMAP을 읽고 필요성을 재판정한다. 과거 NEXT의 구현 예정 문구를 오늘의 실행 승인으로 상속하지 않는다.
